use core::arch::asm;
use core::ptr::*;


const TIMEOUT: usize = 1000;
const BOOT_TIME: usize = 1000000;

pub const SDCARD_BASE: usize = 0x1000_1000;
pub const DATA: *mut u8 = SDCARD_BASE as *mut u8;
pub const CAN_PUT: *const u8 = (SDCARD_BASE + 1) as *const u8;
pub const CAN_PEEK: *const u8 = (SDCARD_BASE + 2) as *const u8;
pub const CS: *mut u8 = (SDCARD_BASE + 3) as *mut u8;
pub const DIV: *mut u32 = (SDCARD_BASE + 4) as *mut u32;

pub fn send(byte: u8) -> u8 {unsafe{
    while read_volatile(CAN_PUT) == 0 {}
    write_volatile(DATA, byte);

    while read_volatile(CAN_PEEK) == 0 {}
    read_volatile(DATA)
}}

pub fn set_div(div: u32) {
    unsafe {write_volatile(DIV, div);}
}

pub fn enable() {
    send(0xff);
    unsafe {write_volatile(CS, 0);}
    send(0xff);
}

pub fn disable() {
    send(0xff);
    unsafe {write_volatile(CS, 1);}
    send(0xff);
}

pub fn send_cmd(cmd: u8, arg: u32, crc: u8) -> Option<u8> {
    send(0xff);
    send(0xff);
    send(cmd | 0x40);
    send(((arg >> 24) & 0xff) as u8);
    send(((arg >> 16) & 0xff) as u8);
    send(((arg >> 8) & 0xff) as u8);
    send((arg & 0xff) as u8);
    send(crc | 0x01);

    for i in 0.. {
        if i > TIMEOUT { break; }

        let x = send(0xff);
        if x != 0xff { return Some(x); }
    }

    return None;
}

pub fn init() {
    println!("start to initialize SD card driver");

    set_div(32);


    disable();

    for _ in 0..BOOT_TIME {
        unsafe { asm!("nop"); }
    }

    enable();

    for _ in 0..160 {
        send(0xff);
    }

    println!("ready to send command 0");

    let x = send_cmd(0, 0, 0x95).unwrap();
    println!("receive: {:x}", x);

    let x = send_cmd(8, 0x01AA, 0x87).unwrap();
    print!("receive: {:x}", x);

    loop {
        let x = send(0xff);
        if x != 0xff {
            print!("{:x}", x);
        } else {
            println!("");
            break;
        }
    }

    let mut x = 0xff;

    while x != 0 {
        println!("cmd 55: {:x}", send_cmd(55, 0, 0).unwrap());
        x = send_cmd(41, 0x40000000, 0).unwrap();
        println!("cmd 41: {:x}", x);
    }
}
