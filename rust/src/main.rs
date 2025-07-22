#![no_std]
#![no_main]
#![feature(alloc_error_handler)]
#![feature(naked_functions)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]
#![allow(dead_code)]

#[macro_use]
extern crate alloc;

#[macro_use]
mod printer;
mod constant;
mod handler;
mod hole;
mod linked_list_allocator;
mod kalloc;
mod mutex;
mod palloc;
mod pointer;
mod process;
mod trap;
mod fixed;
mod screen;
mod vm;
use core::{
    arch::{asm, global_asm},
    panic::PanicInfo,
};

use crate::trap::*;

use riscv::register::{mstatus, mie, minstret, mcycle, mideleg, medeleg};
use riscv::register;

global_asm!(include_str!("init.s"));
global_asm!(include_str!("trampoline.s"));

use alloc::vec::Vec;
use alloc::collections::LinkedList;
use alloc::collections::BTreeSet;
use alloc::boxed::Box;

use crate::pointer::{PhysAddr, PhyPageNum, VirtAddr};

use core::sync::atomic::fence;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("\x1b[31mKERNEL PANIC:\x1b[0m {info}");
    loop {}
}

/// Main program function
#[no_mangle]
unsafe extern "C" fn kernel_main(_hartid: usize, _dtb: usize) -> () {
    let x: usize = 0xffff;
    asm!("csrw mideleg, a0", in("a0") x);
    asm!("csrw medeleg, a0", in("a0") x);

    register::mie::clear_mtimer();

    mstatus::set_mpp(mstatus::MPP::Supervisor);
    mstatus::set_mpie();
    mstatus::clear_mie();
    mstatus::clear_sie();

    register::satp::set(register::satp::Mode::Bare, 0, 0);
    register::mepc::write(supervisor_main as usize);
    asm!("mret");

}

extern "C" {
    pub fn user_binary() -> ();
}

fn setup_initial_user(state: &mut TrapState) {
    let user_stack = palloc::alloc().unwrap();
    let ptable = vm::init_ptable();

    let rwx = vm::Perms{write: true, read: true, exec: true, user: false};
    let urwx = vm::Perms{write: true, read: true, exec: true, user: true};

    for i in 0..50 {
        let page = palloc::alloc().unwrap();
        let mem = PhyPageNum::from((user_binary as usize) / 4096 + i);

        println!("allocate page {}", i);

        page.as_bytes_mut().copy_from_slice(mem.as_bytes_mut());

        vm::map(
            ptable,
            VirtAddr::from(0x1000_0000 + i * 0x1000),
            PhysAddr::from(usize::from(page) * 0x1000),
            4096,
            urwx
        );
    }

    // Allocate trap code
    vm::map(
        ptable,
        VirtAddr::from(trap::run_user as usize & !0xFFF),
        PhysAddr::from(trap::run_user as usize & !0xFFF),
        4096 * 2,
        rwx
    );

    vm::map(
        ptable,
        VirtAddr::from(0x2000_0000),
        PhysAddr::from(usize::from(user_stack) * 4096),
        4096,
        urwx
    );

    let state_addr: usize = state as *const TrapState as usize;
    println!("state address: {:x}", state_addr & !0xFFF);

    vm::map(
        ptable,
        VirtAddr::from(state_addr & !0xFFF),
        PhysAddr::from(state_addr  & !0xFFF),
        4096 * 2,
        rwx
    );

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

    unsafe {
        // mret will set the mode to machine
        //  mstatus::set_mpp(mstatus::MPP::Machine);
        //  mstatus::set_mpie();

        //mie::set_mext();
        //mie::set_mtimer();
    }

    let ptable = vm::init_ptable();
    vm::map(
        ptable,
        VirtAddr::from(0x1000_0000),
        PhysAddr::from(0x1000_0000),
        4096,
        vm::Perms{write: true, read: true, exec: false, user: false}
    );

    vm::map(
        ptable,
        VirtAddr::from(0x8000_0000),
        PhysAddr::from(0x8000_0000),
        33 * 1024 * 1024,
        vm::Perms{write: true, read: true, exec: true, user: false}
    );

    unsafe {
        fence(core::sync::atomic::Ordering::SeqCst);
        register::satp::set(register::satp::Mode::Sv32, 0, usize::from(ptable));
        asm!("sfence.vma zero, zero");
    }

    let user_stack = palloc::alloc().unwrap();

    let mut state: TrapState = TrapState {
        registers: Default::default(),
        kernel_sp: 0,
        satp: 0,
    };

    setup_initial_user(&mut state);

    //println!("user_main: {:x}", state.registers.pc);

    //state.registers.sp = usize::from(user_stack) * 4096 + 4088;
    //state.registers.pc = user_main as usize;


    print!("Hello world!\n");

    loop {
        unsafe { register::sstatus::set_spp(register::sstatus::SPP::User); }
        unsafe { trap::run_user(&mut state) };
        println!("scause: {:?}", register::scause::read().bits());
        handler::handler(&mut state);
    }

}

extern "C" fn user_main() -> () {
    loop {
        print!("Hello users!!!\n");

        //let mut time = 0-mcycle::read();
        //let mut instret = 0-minstret::read();

        //time += mcycle::read();
        //instret += minstret::read();

        //println!("time: {} instret: {}", time, instret);

        unsafe { asm!("ecall") };

        loop {}


        //let x: usize = 0;
        //unsafe {
        //    asm!("cbo.clean 0(a0)", in("a0") x);
        //}
    }
}
