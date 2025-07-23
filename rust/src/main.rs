#![no_std]
#![no_main]
#![feature(alloc_error_handler)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]
#![allow(dead_code)]

#[macro_use]
extern crate alloc;

#[macro_use]
mod printer;
mod constant;
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

use core::{
    arch::{asm, global_asm},
    panic::PanicInfo,
};

use crate::trap::*;

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

fn setup_initial_user(state: &mut Context, file: &[u8], heap_size: usize) {
    let user_stack = palloc::alloc().unwrap();
    let ptable = vm::init_ptable();

    let urw = vm::Perms{write: true, read: true, exec: true, user: false};
    let urwx = vm::Perms{write: true, read: true, exec: true, user: true};

    for i in 0..1 + (file.len() + heap_size) / 0x1000 {
        let page = palloc::alloc().unwrap();

        if file.len() >= i * 0x1000 {
            let a = i * 0x1000;
            let b = core::cmp::min(a + 0x1000, file.len());

            page
                .as_bytes_mut()[0..b-a]
                .copy_from_slice(&file[a..b]);
        }

        vm::map(
            ptable,
            VAddr::from(0x1000_0000 + i * 0x1000),
            PAddr::from(page),
            0x1000,
            urwx,
            false,
            false,
        );
    }

    vm::map(
        ptable,
        VAddr::from(0x2000_0000),
        PAddr::from(user_stack),
        0x1000,
        urw,
        false,
        false,
    );

    vm::map_kernel_memory(ptable);

    state.registers.pc = 0x1000_0000;
    state.registers.sp = 0x2000_0FFC;

    let mut satp = vm::Satp::new();
    satp.set_mode(vm::Mode::Sv32);
    satp.set_ppn(ptable);

    state.satp = usize::from(satp);
}

extern "C" fn  supervisor_main() {
    println!("started!");
    println!("DOoOM os");
    palloc::init();
    kalloc::init();
    trap::init();

    unsafe { register::sstatus::set_spp(register::sstatus::SPP::User); }

    let ptable = vm::init_ptable();
    vm::map(
        ptable,
        VAddr::from(0x1000_0000),
        PAddr::from(0x1000_0000),
        0x1000,
        vm::Perms{write: true, read: true, exec: false, user: false},
        false,
        false,
    );

    vm::map_kernel_memory(ptable);

    unsafe {
        register::satp::set(register::satp::Mode::Sv32, 0, usize::from(ptable));
        asm!("sfence.vma zero, zero");
    }

    let file = include_bytes!("user.bin");
    //setup_initial_user(&mut state, file, 0x10000);

    let perms = vm::Perms{write: true, read: true, exec: true, user: true};
    let mut task = Task::new(KERNEL_ID).unwrap();
    let size = task.map_buffer(VAddr::from(0x1000_0000), file, perms);
    task.map_zeros(VAddr::from(0x1000_0000) + size, 0x10000, perms);

    print!("Hello world!\n");

    loop {
        let state =
            &mut task.context.write();
        unsafe { trap::run_user(state) };
        println!("scause: {:?}", register::scause::read().bits());
        handler::handler(state);
    }
}
