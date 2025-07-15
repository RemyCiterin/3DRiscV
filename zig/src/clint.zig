const Config = @import("config.zig");
const writer = @import("print.zig").writer;

pub const CLINT = extern struct {
    msip: u32,
    _reserved1: [0x4000 - 4]u8,
    mtimecmp: u64,
    _reserved2: [0xBFF8 - 0x4008]u8,
    mtime: u64,
};

pub const msip: *u32 = @ptrFromInt(0x30000000);
pub const mtimecmp: *u64 = @ptrFromInt(0x30000000 + 0x4000);
pub const mtime: *u64 = @ptrFromInt(0x30000000 + 0xbff8);

pub const clint: *volatile CLINT = @ptrFromInt(0x30000000);

pub fn setNextTimerInterrupt() void {
    clint.mtimecmp = clint.mtime + Config.timer_step;
}

pub fn getTime() u64 {
    return clint.mtime;
}
