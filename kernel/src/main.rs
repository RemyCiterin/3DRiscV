#![no_std]
#![no_main]
#![feature(alloc_error_handler)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]
#![feature(get_mut_unchecked)]
#![allow(dead_code)]

#[macro_use]
extern crate alloc;

#[macro_use]
mod printer;
mod params;
mod handler;
mod kalloc;
mod palloc;
mod pointer;
mod process;
mod trap;
mod fixed;
mod screen;
mod vm;
mod channel;
mod object;
mod task;
mod syscall;
mod mapping;
mod scheduler;

use core::{
    arch::{asm, global_asm},
    panic::PanicInfo,
};

use riscv::register;

global_asm!(include_str!("init.s"));
global_asm!(include_str!("trampoline.s"));

use crate::pointer::{PAddr, VAddr};

use object::*;
use task::*;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("\x1b[31mKERNEL PANIC:\x1b[0m {info}");
    loop {}
}

/// Main program function
#[no_mangle]
unsafe extern "C" fn machine_main() -> () {
    let x: usize = 0xffff;
    asm!("csrw mideleg, a0", in("a0") x);
    asm!("csrw medeleg, a0", in("a0") x);

    register::mie::clear_mtimer();

    register::mstatus::set_mpp(register::mstatus::MPP::Supervisor);
    register::mstatus::set_mpie();
    register::mstatus::clear_mie();
    register::mstatus::clear_sie();

    register::satp::set(register::satp::Mode::Bare, 0, 0);
    register::mepc::write(supervisor_main as usize);
    asm!("mret");
}

extern "C" fn  supervisor_main() {
    println!("started!");
    println!("DOoOM os");
    palloc::init();
    kalloc::init();
    trap::init();

    unsafe { register::sstatus::set_spp(register::sstatus::SPP::User); }

    let ptable = vm::PTable::new();
    ptable.map_global(
        VAddr::from(0x1000_0000),
        &vm::Frame::from(PAddr::from(0x1000_0000)),
        params::PAGE_SIZE,
        vm::Perms{write: true, read: true, exec: false, user: false},
    );

    ptable.map_ram();

    unsafe {
        register::satp::write(usize::from(ptable.satp(0)));
        asm!("sfence.vma zero, zero");
    }

    let file = include_bytes!("user.bin");

    let perms = vm::Perms{write: true, read: true, exec: true, user: true};
    let task = Task::new(KERNEL_ID).unwrap();
    let size = task.map_buffer(VAddr::from(0x5000), file, perms);
    task.map_zeros(VAddr::from(0x5000) + size, 0x10000, perms);

    let scheduler = scheduler::Scheduler::new();

    scheduler.push(alloc::sync::Arc::new(task));

    print!("{} pages allocated and {} are still busy\n", palloc::count(), palloc::size());

    loop {
        if let Some(task) = scheduler.choose_task() {
            unsafe { trap::run_user(&mut task.context.write()) };
            task.to_idle();

            syscall::handle_syscall(&scheduler, task.clone());
            task.context.write().registers.pc += 4;
        } else {
            scheduler.canonicalize();
        }
    }
}
