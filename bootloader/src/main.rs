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
mod pointer;
mod malloc;
mod sdcard;
mod screen;

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
unsafe extern "C" fn bootloader_main() -> () {
    malloc::init();
    println!("Hello world!");

    let pixel = screen::Pixel::new(255, 0, 255);

    for i in 0..320 {
        for j in 0..240 {
            pixel.write_frame_buffer(i, j);
        }
    }

    sdcard::init();
    loop {}
}

