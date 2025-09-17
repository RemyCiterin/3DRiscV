const std = @import("std");
const UART: usize = 0x10000000;

// THR:transmitter holding register
const UART_THR: *volatile u8 = @ptrFromInt(UART + 0x00);

// LSR:line status register
const UART_LSR: *volatile u8 = @ptrFromInt(UART + 0x05);

// LSR Bit 6: Transmitter empty; both the THR and LSR are empty
const UART_LSR_EMPTY_MASK: usize = 0x40;

const UART_CAN_PUT: *volatile u8 = @ptrFromInt(UART + 1);

export var tohost: u64 = 0;
export var htif: u64 = 0;
export var fromhost: u64 = 0;

const config = @import("config.zig").config;

pub inline fn putChar(ch: u8) void {
    //while ((UART_LSR.* & UART_LSR_EMPTY_MASK) == 0) {}
    while (UART_CAN_PUT.* == 0) {}
    UART_THR.* = ch;
}

pub fn getChar() u8 {
    //  if (UART_LSR.* & 1 == 0) return null;
    return UART_THR.*;
}

fn writeFn(_: void, bytes: []const u8) error{}!usize {
    if (bytes.len == 0) return 0;

    putChar(bytes[0]);
    return 1;
}

pub fn putString(str: []const u8) void {
    for (str) |ch| {
        putChar(ch);
    }
}

pub const Writer = std.io.Writer(void, error{}, writeFn);
pub const writer: Writer = .{ .context = {} };
