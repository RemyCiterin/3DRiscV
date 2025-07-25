#![no_std]
#![no_main]
#![feature(alloc_error_handler)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]
#![allow(dead_code)]

// #[macro_use]
extern crate alloc;

#[macro_use]
mod printer;
mod malloc;

use core::arch::asm;

use core::{
    arch::{global_asm},
    panic::PanicInfo,
};

global_asm!(include_str!("init.s"));

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("\x1b[31mKERNEL PANIC:\x1b[0m {info}");
    loop { yield_syscall(); }
}

pub fn fork() -> usize {
    let mut name = 0;
    let mut code = 1;

    unsafe {asm!("ecall", inout("a0") code, inout("a1") name); }
    assert!(code == 0);

    return name;
}

pub fn pipe() -> (usize, usize) {
    let mut source = 0;
    let mut sink = 0;
    let mut code = 4;

    unsafe {asm!("ecall", inout("a0") code, inout("a1") source, inout("a2") sink);}
    assert!(code == 0);

    return (source, sink);
}

pub const IPC_BUFFER: *mut usize = 0x3000 as *mut usize;

pub fn send(sink: usize, x: usize) {
    let mut code = 2;

    unsafe{
        core::ptr::write_volatile(IPC_BUFFER, x);
        asm!("ecall", inout("a0") code, in("a1") sink);
    }

    assert!(code == 0);
}

pub fn receive(source: usize) -> usize {
    let mut code = 3;

    unsafe{
        asm!("ecall", inout("a0") code, in("a1") source);
        assert!(code == 0);

        return core::ptr::read_volatile(IPC_BUFFER);
    }
}

pub fn yield_syscall() {
    let mut code = 0;

    unsafe{
        asm!("ecall", inout("a0") code);
        assert!(code == 0);
    }
}

/// Main program function
#[no_mangle]
unsafe extern "C" fn user_main() -> () {
    malloc::init();

    println!("Hello world!");

    let (source, sink) = pipe();
    println!("source: {} sink: {}", source, sink);

    let id = fork();

    if id == 0 {
        println!("Hello world! from process {}", id);
        send(sink, 20);
    } else {
        let x = receive(source);
        println!("{}", x);
        println!("Hello world! from process {}", id);
    }

    loop { yield_syscall(); }
}
