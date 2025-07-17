const Config = @import("config.zig");
const writer = @import("print.zig").writer;

pub const msip: *volatile u32 = @ptrFromInt(0x30000000);
pub const mtimecmp: *volatile u64 = @ptrFromInt(0x30000000 + 0x4000);
pub const mtime: *volatile u64 = @ptrFromInt(0x30000000 + 0xbff8);

pub fn setNextTimerInterrupt() void {
    mtimecmp.* = mtime.* +% Config.timer_step;
}

pub fn getTime() u64 {
    return mtime.*;
}
