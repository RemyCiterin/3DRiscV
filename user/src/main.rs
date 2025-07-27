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

// TODO: remove magic numbers

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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Source(usize);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Sink(usize);

pub fn fork() -> bool {
    let mut name = 0;
    let mut code = 1;

    unsafe {asm!("ecall", inout("a0") code, inout("a1") name); }
    assert!(code == 0);

    return name == 0;
}

pub fn pipe() -> (Source, Sink) {
    let mut source = 0;
    let mut sink = 0;
    let mut code = 4;

    unsafe {asm!("ecall", inout("a0") code, inout("a1") source, inout("a2") sink);}
    assert!(code == 0);

    return (Source(source), Sink(sink));
}

pub const IPC_BASE: usize = 0x3000;
pub const IPC_BUFFER_USIZE: *mut usize = IPC_BASE as *mut usize;
pub const IPC_BUFFER_U8: *mut u8 = IPC_BASE as *mut u8;

pub fn server<F>(source: Source, mut f: F) where F:FnMut(&[u8]) -> () {
    loop { receive(source, &mut f); }
}

pub fn receive<F, T>(source: Source, mut f: F) -> T where F: FnMut(&[u8]) -> T {
    let mut code: usize = 3;
    let mut length: usize;
    unsafe { asm!("ecall", inout("a0") code, in("a1") source.0, out("a2") length); }

    // Ensure that the call succeded
    assert!(code == 0);

    f(unsafe{ core::slice::from_raw_parts(IPC_BUFFER_U8, length) })
}

pub fn send_bytes(sink: Sink, bytes: &[u8]) {
    unsafe {
        let length = bytes.len();
        for (i, &c) in bytes.iter().enumerate() {
            core::ptr::write_volatile((IPC_BASE+i) as *mut u8, c);
        }

        let mut code: usize = 2;
        asm!("ecall", inout("a0") code, in("a1") sink.0, in("a2") length);
        assert!(code == 0);
    }
}

pub fn send_string(sink: Sink, string: &str) {
    send_bytes(sink, &string.as_bytes());
}

#[no_mangle]
pub extern "C" fn yield_syscall() {
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

    let (source1, sink1) = pipe();
    println!("generate source {:?} and sink {:?}", source1, sink1);

    if fork() {
        server(source1, |x| {
            println!("receive: {:?}", core::str::from_utf8(x));
        });
    } else {
        let (source2, sink2) = pipe();
        println!("generate source {:?} and sink {:?}", source2, sink2);
        if fork() {
            send_string(sink1, "Hello world!");
            send_string(sink1, "This message");
            send_string(sink1, "was sent using");
            send_string(sink2, "Start task 7");
        } else {
            receive(source2, |_| {
                send_string(sink1, "interprocess");
                send_string(sink1, "communication!");
            });
        }
    }
}
