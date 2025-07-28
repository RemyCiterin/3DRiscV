use bitflags::bitflags;

use crate::{params::*, palloc, pointer::*};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mode { Bare, Sv32 }

#[derive(Copy, Clone, Debug)]
pub struct Satp { bits: usize }

impl Satp {
    pub fn new() -> Self {
        Self::from(0)
    }

    pub fn mode(&self) -> Mode {
        if self.bits & (1 << 31) == 0 {
            Mode::Sv32
        } else {
            Mode::Bare
        }
    }

    pub fn set_mode(&mut self, mode: Mode) {
        if mode == Mode::Bare {
            self.bits &= !(1 << 31);
        } else {
            self.bits |= 1 << 31;
        }
    }

    pub fn asid(&self) -> usize {
        let mask: usize = (1 << 9) - 1;
        (self.bits >> 22) & mask
    }

    pub fn set_asid(&mut self, asid: usize) {
        let mask: usize = (1 << 9) - 1;

        assert!(asid & !mask == 0);
        self.bits = (self.bits & !(mask << 22)) | asid << 22;
    }

    pub fn ppn(&self) -> Page {
        Page::from(self.bits & ((1 << 22) - 1))
    }

    pub fn set_ppn(&mut self, ppn: Page) {
        let mask: usize = (1 << 22) - 1;

        assert!(usize::from(ppn) & !mask == 0);
        self.bits = (self.bits & !mask) | usize::from(ppn);
    }
}

impl From<usize> for Satp {
    fn from(bits: usize) -> Self {
        Self { bits }
    }
}

impl From<Satp> for usize {
    fn from(satp: Satp) -> usize {
        satp.bits
    }
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Flags: usize {
        const V = 1 << 0;
        const R = 1 << 1;
        const W = 1 << 2;
        const X = 1 << 3;
        const U = 1 << 4;
        const G = 1 << 5;
        const A = 1 << 6;
        const D = 1 << 7;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Perms {
    pub write: bool,
    pub read: bool,
    pub exec: bool,
    pub user: bool,
}

// Definition of a Page Table Entry
#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Entry(usize);

impl Entry {
    fn new(ppn: Page, flags: Flags) -> Self {
        Self(flags.bits() | (usize::from(ppn) << 10))
    }

    fn ppn(&self) -> Page {
        Page::from(self.0 >> 10)
    }

    fn flags(&self) -> Flags {
        Flags::from_bits(self.0 & ((1 << 10) - 1)).unwrap()
    }

    fn perms(&self) -> Perms {
        Perms {
            write: self.flags().contains(Flags::W),
            read: self.flags().contains(Flags::R),
            exec: self.flags().contains(Flags::X),
            user: self.flags().contains(Flags::U),
        }
    }

    fn set_ppn(&mut self, ppn: Page) {
        *self = Self::new(ppn, self.flags())
    }

    fn set_perms(&mut self, perms: Perms) {
        let mut flags = self.flags();

        flags = if perms.write {
            flags | Flags::W
        } else {
            flags & !Flags::W
        };
        flags = if perms.read {
            flags | Flags::R
        } else {
            flags & !Flags::R
        };
        flags = if perms.exec {
            flags | Flags::X
        } else {
            flags & !Flags::X
        };
        flags = if perms.user {
            flags | Flags::U
        } else {
            flags & !Flags::U
        };

        *self = Self::new(self.ppn(), flags);
    }

    fn valid(&self) -> bool {
        self.flags().contains(Flags::V)
    }

    fn leaf(&self) -> bool {
        let p = self.perms();
        p.exec || p.read || p.write
    }

    fn clear_valid(&mut self) {
        *self = Self::new(self.ppn(), self.flags() & !Flags::V)
    }

    fn set_valid(&mut self) {
        *self = Self::new(self.ppn(), self.flags() | Flags::V)
    }

    fn set_global(&mut self) {
        *self = Self::new(self.ppn(), self.flags() | Flags::G);
    }

    fn clear_global(&mut self) {
        *self = Self::new(self.ppn(), self.flags() & !Flags::G);
    }
}

impl Default for Entry {
    fn default() -> Self {
        Entry(0)
    }
}

// allocate and inite a page table
fn init_ptable() -> Page {
    let ppn: Page = palloc::alloc().unwrap();

    let ptable: &'static mut [Entry] = ppn.as_pte_mut();
    ptable.fill(Default::default());
    return ppn;
}

/// Return the last level page table entry associated with a vitrual address
/// and allocate if necessary some new intermediate page tables
fn walk_and_alloc(
    mut ptable: Page,
    va: VAddr,
    megapage: bool
) -> (&'static mut Entry, usize) {
    let mut level = 1;

    loop {
        let entry: &'static mut Entry = &mut ptable.as_pte_mut()[va.vpn(level)];

        if level == 0 || entry.leaf() {
            return (entry, level);
        }

        if !entry.valid() {
            if megapage { return (entry, level) }

            *entry = Entry::new(init_ptable(), Flags::V);
        }

        ptable = entry.ppn();
        level -= 1;
    }
}

/// Recursively free a page table, if free_leaf_ppn is set
/// then it also free the pages mapped by the page table
fn free_ptable(ptable: Page, free_leaf_ppn: bool) {
    let entries: &'static mut[Entry] = ptable.as_pte_mut();

    for &entry in entries.iter() {
        if entry.valid() && !entry.leaf() {
            free_ptable(entry.ppn(), free_leaf_ppn);
        } else if entry.leaf() && free_leaf_ppn {
            palloc::free(entry.ppn());
        }
    }

    palloc::free(ptable);
}

/// Return the last level page table entry associated with a virtual address
/// or null if the address is not allocated
fn walk(
    mut ptable: Page,
    va: VAddr,
) -> (&'static mut Entry, usize) {
    let mut level: usize = 1;

    loop {
        let entry: &'static mut Entry = &mut ptable.as_pte_mut()[va.vpn(level)];

        if level == 0 || !entry.valid() || entry.leaf() {
            return (entry, level);
        }

        ptable = entry.ppn();
        level -= 1;
    }
}

fn map(
    ptable: Page,
    mut virt: VAddr,
    mut phys: PAddr,
    size: usize,
    perms: Perms,
    global: bool,
    allow_megapage: bool
) {
    if virt.offset() != 0 || phys.offset() != 0 {
        panic!(
            "map: alignment error phys: {:?} virt: {:?}",
            virt, virt
        );
    }

    if size % PAGE_SIZE != 0 {
        panic!(
            "map: size must be divisible by {}, found {}",
            PAGE_SIZE,
            size
        );
    }

    println!("map [{:x}, {:x}] to [{:x}, {:x}]",
        usize::from(phys), usize::from(phys)+size-1,
        usize::from(virt), usize::from(virt)+size-1,
    );

    let last = virt + size - PAGE_SIZE;

    loop {
        let va: usize = usize::from(virt);
        let pa: usize = usize::from(phys);
        let mut megapage: bool =
            allow_megapage && usize::from(last) >= va + MEGAPAGE_SIZE &&
            pa % MEGAPAGE_SIZE == 0 && va % MEGAPAGE_SIZE == 0;

        let (entry, level) =
            walk_and_alloc(ptable, virt, megapage);
        megapage = level == 1;

        if entry.valid() {
            panic!("remap");
        }

        if global { entry.set_global(); }
        entry.set_ppn(phys.ppn());
        entry.set_perms(perms);
        entry.set_valid();

        if virt == last { return; }

        virt += if megapage {MEGAPAGE_SIZE} else {PAGE_SIZE};
        phys += if megapage {MEGAPAGE_SIZE} else {PAGE_SIZE};
    }
}

/// unmap a set of address from a page table, `unmap` may fail if:
/// - `virt` is not aligned on a page
/// - `size` is not divisible by the size of a page
/// - we try to unmap an unaligned address
/// - we try to unmap a megapage but `virt` is not aligned on a megapage
/// - we try to unmap a megapage but it is not fully include in the input region
///
/// If free is set then we also free the pages that we unmap
fn unmap(ptable: Page, mut virt: VAddr, size: usize, free: bool) {
    if virt.offset() != 0 {
        panic!("unmap: alignment error: {:?}", virt);
    }

    if size % PAGE_SIZE != 0 {
        panic!(
            "unmap: size must be divisible by {}, found {}",
            PAGE_SIZE,
            size
        );
    }

    let last = virt + size - PAGE_SIZE;

    loop {
        let va: usize = usize::from(virt);
        let (entry, level) = walk(ptable, virt);
        let megapage = level == 1;

        if !entry.valid() || !entry.leaf() { panic!("unmap"); }
        if megapage && va % MEGAPAGE_SIZE != 0 { panic!("unmap megapage"); }
        if megapage && last < virt + MEGAPAGE_SIZE { panic!("unmap megapage"); }
        if !entry.flags().contains(Flags::G) { panic!("unmap global") }

        if free { palloc::free(entry.ppn()); }
        *entry = Default::default();

        if virt == last { return; }
        virt += if megapage {MEGAPAGE_SIZE} else {PAGE_SIZE};
    }
}

/// Map RAM memory as supervisor memory, used by users to ensure `trampoline.s` and
/// their context are defined by their ptage table
fn map_ram(ptable: Page) {
    let perms = Perms{exec: true, read: true, write: true, user: false};

    map(
        ptable,
        VAddr(RAM_BEGIN),
        PAddr(RAM_BEGIN),
        RAM_SIZE,
        perms,
        true,
        true
    );
}

/// A frame is a pointer to a memory page, it can be initialized using a
/// conversion from an usize, or it can be initialized from the page allocator
/// using `Frame::alloc()`, in this case the page is free at the deinitialisation
/// of the frame using the `Drop` trait
pub struct Frame{
    page: Page,
    from_palloc: bool,
}

impl From<usize> for Frame {
    fn from(x: usize) -> Frame {
        Self { page: Page::from(x), from_palloc: false }
    }
}

impl From<PAddr> for Frame {
    fn from(a: PAddr) -> Frame {
        assert!(usize::from(a) % PAGE_SIZE == 0);
        Self{ page: Page::from(usize::from(a) / PAGE_SIZE), from_palloc: false }
    }
}

impl From<&Frame> for PAddr {
    fn from(x: &Frame) -> PAddr { PAddr::from(x.page) }
}

impl Frame {
    /// Allocate a frame, in this case the frame will be free
    /// when the frame will be drop
    pub fn alloc() -> Option<Self> {
        Some(Self{
            page: palloc::alloc()?,
            from_palloc: true,
        })
    }

    pub fn as_bytes(&self) -> &'static [u8] {
        self.page.as_bytes()
    }

    pub fn as_bytes_mut(&self) -> &'static mut [u8] {
        self.page.as_bytes_mut()
    }
}

impl Drop for Frame {
    fn drop(&mut self) {
        if self.from_palloc { palloc::free(self.page); }
    }
}

/// A generic page table type, the memory allocated for this page table is recursively free
/// using the `Drop` trait.
pub struct PTable(Page);

impl PTable {
    pub fn new() -> Self {
        PTable(init_ptable())
    }

    /// Map memory locally (those entries can be GC using sfence.vma, and can't use megapages)
    pub fn map_local(&self, virt: VAddr, frame: &Frame, size: usize, perms: Perms) {
        map(self.0, virt, PAddr::from(frame), size, perms, false, false);
    }

    /// Map memory globally (this memory can use megapages)
    pub fn map_global(&self, virt: VAddr, frame: &Frame, size: usize, perms: Perms) {
        map(self.0, virt, PAddr::from(frame), size, perms, true, true);
    }

    /// Map all the RAM memory into the address space defined by this frame as supervisor memory
    pub fn map_ram(&self) {
        map_ram(self.0)
    }

    /// Unmap memory
    pub fn unmap(&self, virt: VAddr, size: usize) {
        unmap(self.0, virt, size, false);
    }

    pub fn satp(&self, asid: usize) -> Satp {
        let mut satp = Satp::new();
        satp.set_mode(Mode::Sv32);
        satp.set_ppn(self.0);
        satp.set_asid(asid);
        satp
    }
}

impl Drop for PTable {
    fn drop(&mut self) {
        free_ptable(self.0, false)
    }
}
