use core::alloc::Layout;

use linked_list_allocator::LockedHeap;

pub const MALLOC_SIZE: usize = 0x1000;

#[global_allocator]
pub static MAIN_ALLOCATOR: LockedHeap = LockedHeap::empty();

extern "C" {
    static mut MALLOC_BUFFER: [u8; MALLOC_SIZE];
}

#[allow(static_mut_refs)]
pub fn init() {
    unsafe {
        println!("kalloc buffer base: {:p}", MALLOC_BUFFER.as_ptr());
        MAIN_ALLOCATOR
            .lock()
            .init(MALLOC_BUFFER.as_mut_ptr(), MALLOC_SIZE);
    }
}

#[alloc_error_handler]
pub fn handle_alloc_error(layout: Layout) -> ! {
    panic!("kalloc fail: {:?}", layout);
}
