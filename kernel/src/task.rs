use crate::pointer::{Page, PAddr, VAddr};
use crate::object::*;
use crate::params::*;

use alloc::sync::Arc;
use spinning_top::Spinlock;
use spinning_top::RwSpinlock;
use crate::vm;
use crate::palloc;
use crate::trap::Context;
use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use core::sync::atomic::AtomicU32;
use core::sync::atomic::Ordering;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Capability(u32);

impl From<usize> for Capability {
    fn from(x: usize) -> Capability { Capability(x as u32) }
}

impl From<Capability> for usize {
    fn from(x: Capability) -> usize { x.0 as usize }
}

#[derive(Clone)]
pub enum State {
    /// The process is blocked because if wait to receive data from a source,
    /// or to send data to a sink
    Blocked(Arc<dyn Object>),
    Idle,
    Busy,
}

pub struct Task {
    /// Unique task id
    pub id: ObjectId,

    /// List of the objects that can be accessed by a task
    pub capabilities: Spinlock<BTreeMap<Capability, Arc<dyn Object>>>,

    /// Manage capabilities allocation
    next_capa: AtomicU32,

    /// Mapping from virtual addresses to physical addresses
    pub mapping: RwSpinlock<Vec<(VAddr, Page)>>,

    /// Root page table of the process
    pub ptable: Spinlock<Page>,

    /// State of the process
    pub state: RwSpinlock<State>,

    /// Interprocess-communication-buffer
    pub ipc_buffer: Spinlock<Page>,

    /// Parent of the task
    pub owner: ObjectId,

    /// Thread context
    pub context: RwSpinlock<Context>,
}

unsafe impl Send for Task {}
unsafe impl Sync for Task {}

impl Object for Task {
    fn id(&self) -> ObjectId { self.id }
}

pub struct TaskConfig {
    /// Virtual address of the inter-process communication buffer
    ipc: usize,

    /// Virtual address of the initial program counter
    pc: usize,

    /// Virtual address of the stack
    stack: usize,
}

impl Task {
    pub fn new(owner: ObjectId) -> Option<Self> {
        let stack = palloc::alloc()?;
        Task::new_with_stack(owner, stack)
    }

    pub fn new_with_stack(owner: ObjectId, stack: Page) -> Option<Self> {
        let ipc_buffer = palloc::alloc()?;
        let ptable = vm::init_ptable();

        // Ensure the content of trampoline.s and the context are visible as
        // supervisor memory in the user address space
        vm::map_kernel_memory(ptable);

        let urw = vm::Perms{read: true, write: true, exec: false, user: true};

        let mapping = vec![
            (VAddr::from(0x2000), stack),
            (VAddr::from(0x3000), ipc_buffer),
        ];

        for &(virt, phys) in mapping.iter() {
            vm::map(
                ptable,
                virt,
                PAddr::from(phys),
                PAGE_SIZE,
                urw,
                false,
                false
            );
        }

        let mut context: Context = Default::default();

        context.registers.pc = 0x5000;
        context.registers.sp = 0x2FF0;
        let mut satp = vm::Satp::new();
        satp.set_mode(vm::Mode::Sv32);
        satp.set_ppn(ptable);
        context.satp = usize::from(satp);

        Some(Self {
            owner,
            id: new_object_id(),
            capabilities: Spinlock::new(BTreeMap::new()),
            next_capa: AtomicU32::new(0),
            mapping: RwSpinlock::new(mapping),
            ptable: Spinlock::new(ptable),
            state: RwSpinlock::new(State::Idle),
            ipc_buffer: Spinlock::new(ipc_buffer),
            context: RwSpinlock::new(context)
        })
    }

    pub fn fork(&self) -> Option<Task> {
        let stack = palloc::alloc()?;

        for (virt, page) in self.mapping.read().iter() {
            if virt == &VAddr::from(0x2000) {
                stack.as_bytes_mut().copy_from_slice(page.as_bytes());
            }
        }

        let task = Task::new_with_stack(self.id(), stack)?;
        task.next_capa.store(
            self.next_capa.load(Ordering::Relaxed),
            Ordering::Relaxed
        );

        for (virt, page) in self.mapping.read().iter() {
            if virt == &VAddr::from(0x2000) { continue; }
            if virt == &VAddr::from(0x3000) { continue; }

            let perms = vm::Perms{user: true, read: true, write: true, exec: true};
            task.map_buffer(virt.clone(), page.as_bytes(), perms);
        }

        Some(task)
    }

    pub fn map_buffer(&self, mut virt: VAddr, buf: &[u8], perms: vm::Perms)
        -> usize {
        let mut num_pages = buf.len() / PAGE_SIZE;
        if buf.len() % PAGE_SIZE != 0 { num_pages += 1; }

        let mapping =
            &mut self.mapping.write();
        let ptable =
            self.ptable.lock().clone();

        for i in 0..num_pages {
            let page = palloc::alloc().unwrap();

            if buf.len() >= i * PAGE_SIZE {
                let a = i * PAGE_SIZE;
                let b = core::cmp::min(a + PAGE_SIZE, buf.len());

                page
                    .as_bytes_mut()[0..b-a]
                    .copy_from_slice(&buf[a..b]);

                page
                    .as_bytes_mut()[b-a..PAGE_SIZE]
                    .fill(0);
            }

            vm::map(
                ptable,
                virt,
                PAddr::from(page),
                PAGE_SIZE,
                perms,
                false,
                false,
            );

            mapping.push((virt, page));

            virt += PAGE_SIZE;
        }

        return num_pages * PAGE_SIZE;
    }

    pub fn map_zeros(&self, mut virt: VAddr, size: usize, perms: vm::Perms) {
        assert!(size % PAGE_SIZE == 0);

        let mapping =
            &mut self.mapping.write();
        let ptable =
            self.ptable.lock().clone();

        for _ in 0..size / PAGE_SIZE {
            let page = palloc::alloc().unwrap();

            page
                .as_bytes_mut()
                .fill(0);

            vm::map(
                ptable,
                virt,
                PAddr::from(page),
                PAGE_SIZE,
                perms,
                false,
                false,
            );

            mapping.push((virt, page));

            virt += PAGE_SIZE;
        }
    }

    /// Return a new capability for a given object
    pub fn new_capability(&self, obj: Arc<dyn Object>) -> Capability {
        let new_capa = self.next_capa.fetch_add(1, Ordering::SeqCst);
        self.capabilities.lock().insert(Capability(new_capa), obj);
        return Capability(new_capa);
    }

    /// Generate a new pipe
    pub fn new_pipe(&self) -> (Capability, Capability) {
        let (source, sink) =
            crate::channel::new_channel(self.id);

        let c1: Capability = self.new_capability(source);
        let c2: Capability = self.new_capability(sink);

        (c1, c2)
    }

    pub fn to_blocked(&self, obj: Arc<dyn Object>) {
        self.state.write().clone_from(&State::Blocked(obj));
    }

    pub fn to_idle(&self) {
        self.state.write().clone_from(&State::Idle);
    }

    pub fn to_busy(&self) {
        self.state.write().clone_from(&State::Busy);
    }
}

/// Try to send a message to a channel, return None in case of a failure,
/// this procedure can set the current task as blocked and unlock another thread
pub fn send(task: Arc<Task>, capability: Capability, size: usize) -> Option<()> {
    let arc = task.capabilities.lock().get(&capability).cloned()?;

    let sink =
        arc.downcast_arc::<crate::channel::Sink>().ok()?;

    let send_to = sink.send(task.clone(), size).ok()?;

    if let Some(obj) = send_to {
        let other =
            &obj.clone().downcast_arc::<Task>().ok()?;
        let buf1 = task.ipc_buffer.lock();
        let buf2 = other.ipc_buffer.lock();

        other.context.write().registers.a2 = size;
        other.to_idle();
        buf2
            .as_bytes_mut()[0..size]
            .copy_from_slice(&buf1.as_bytes()[0..size]);
    } else {
        task.to_blocked(sink.clone());
    }

    Some(())
}

/// Try to receive a message from a channel, return None in case of a failure,
/// this procedure can set the current task as blocked and unlock another thread
pub fn receive(task: Arc<Task>, capability: Capability) -> Option<usize> {
    let arc = task.capabilities.lock().get(&capability).cloned()?;

    let source =
        arc.downcast_arc::<crate::channel::Source>().ok()?;

    let send_to = source.receive(task.clone()).ok()?;

    if let Some((obj, size)) = send_to {
        let other =
            &obj.clone().downcast_arc::<Task>().ok()?;
        let buf1 = task.ipc_buffer.lock();
        let buf2 = other.ipc_buffer.lock();

        other.to_idle();
        buf1
            .as_bytes_mut()[0..size]
            .copy_from_slice(&buf2.as_bytes()[0..size]);
        Some(size)
    } else {
        task.to_blocked(source.clone());

        // Return a fake size because this size will be overwriten during
        // the coresponding send
        Some(0)
    }
}

impl Drop for Task {
    fn drop(&mut self) {
        let ptable = self.ptable.lock();
        let maps = self.mapping.read();

        for &(virt, _) in maps.iter() {
            vm::unmap(ptable.clone(), virt, PAGE_SIZE, true);
        }

        vm::free_ptable(ptable.clone(), false);
    }
}
