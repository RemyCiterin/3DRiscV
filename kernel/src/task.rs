use crate::pointer::{Page, PAddr, VAddr};
use crate::object::*;
use crate::channel;

use alloc::sync::Arc;
use alloc::sync::Weak;
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
    pub ipc_buffer: RwSpinlock<Page>,

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
        let ipc_buffer = palloc::alloc()?;
        let ptable = vm::init_ptable();
        let stack = palloc::alloc()?;

        // Ensure the content of trampoline.s and the context are visible as
        // supervisor memory in the user address space
        vm::map_kernel_memory(ptable);

        let urw = vm::Perms{read: true, write: true, exec: false, user: true};

        let mapping = vec![
            (VAddr::from(0x2000_0000), stack),
            (VAddr::from(0x3000), ipc_buffer),
        ];

        for &(virt, phys) in mapping.iter() {
            vm::map(
                ptable,
                virt,
                PAddr::from(phys),
                vm::PSIZE,
                urw,
                false,
                false
            );
        }

        let mut context: Context = Default::default();

        context.registers.pc = 0x1000_0000;
        context.registers.sp = 0x2000_0FFC;
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
            ipc_buffer: RwSpinlock::new(ipc_buffer),
            context: RwSpinlock::new(context)
        })
    }

    pub fn map_buffer(&mut self, mut virt: VAddr, buf: &[u8], perms: vm::Perms) -> usize {
        let mut num_pages = buf.len() / vm::PSIZE;
        if buf.len() & vm::PMASK != 0 { num_pages += 1; }

        let mapping =
            &mut self.mapping.write();
        let ptable =
            self.ptable.lock().clone();

        for i in 0..num_pages {
            let page = palloc::alloc().unwrap();

            if buf.len() >= i * vm::PSIZE {
                let a = i * vm::PSIZE;
                let b = core::cmp::min(a + vm::PSIZE, buf.len());

                page
                    .as_bytes_mut()[0..b-a]
                    .copy_from_slice(&buf[a..b]);

                page
                    .as_bytes_mut()[b-a..vm::PSIZE]
                    .fill(0);
            }

            vm::map(
                ptable,
                virt,
                PAddr::from(page),
                vm::PSIZE,
                perms,
                false,
                false,
            );

            mapping.push((virt, page));

            virt += vm::PSIZE;
        }

        return num_pages * vm::PSIZE;
    }

    pub fn map_zeros(&mut self, mut virt: VAddr, size: usize, perms: vm::Perms) {
        assert!(size % vm::PSIZE == 0);

        let mapping =
            &mut self.mapping.write();
        let ptable =
            self.ptable.lock().clone();

        for i in 0..size / vm::PSIZE {
            let page = palloc::alloc().unwrap();

            page
                .as_bytes_mut()
                .fill(0);

            vm::map(
                ptable,
                virt,
                PAddr::from(page),
                vm::PSIZE,
                perms,
                false,
                false,
            );

            mapping.push((virt, page));

            virt += vm::PSIZE;
        }
    }

    /// Return a new capability for a given object
    pub fn new_capability(&mut self, obj: Arc<dyn Object>) -> Capability {
        let new_capa = self.next_capa.fetch_add(1, Ordering::SeqCst);
        self.capabilities.lock().insert(Capability(new_capa), obj);
        return Capability(new_capa);
    }
}

impl Drop for Task {
    fn drop(&mut self) {
        let ptable = self.ptable.lock();
        let maps = self.mapping.read();

        for &(virt, phys) in maps.iter() {
            vm::unmap(ptable.clone(), virt, vm::PSIZE, true);
        }

        vm::free_ptable(ptable.clone(), false);
    }
}
