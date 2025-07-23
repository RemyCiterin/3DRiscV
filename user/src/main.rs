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

use core::{
    arch::{global_asm},
    panic::PanicInfo,
};

global_asm!(include_str!("init.s"));

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("\x1b[31mKERNEL PANIC:\x1b[0m {info}");
    loop {}
}

/// Main program function
#[no_mangle]
unsafe extern "C" fn user_main() -> () {
    println!("Hello world!");
    println!("Hello world!");
    println!("Hello world!");
    println!("Hello world!");
}

