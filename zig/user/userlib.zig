const std = @import("std");

pub const SyscallArg = struct {
    a0: usize = undefined,
    a1: usize = undefined,
    a2: usize = undefined,
    a3: usize = undefined,
    a4: usize = undefined,
    a5: usize = undefined,
    a6: usize = undefined,
    a7: usize = undefined,
};

pub fn syscall6(args: SyscallArg) SyscallArg {
    var a0 = args.a0;
    var a1 = args.a1;
    var a2 = args.a2;
    var a3 = args.a3;
    var a4 = args.a4;
    var a5 = args.a5;
    var a6 = args.a6;
    var a7 = args.a7;

    asm volatile (
        \\ecall
        : [a0] "+{a0}" (a0),
          [a1] "+{a1}" (a1),
          [a2] "+{a2}" (a2),
          [a3] "+{a3}" (a3),
          [a4] "+{a4}" (a4),
          [a5] "+{a5}" (a5),
          [a6] "+{a6}" (a6),
          [a7] "+{a7}" (a7),
    );

    return .{
        .a0 = a0,
        .a1 = a1,
        .a2 = a2,
        .a3 = a3,
        .a4 = a4,
        .a5 = a5,
        .a6 = a6,
        .a7 = a7,
    };
}

pub const SyscallCode = enum(usize) {
    PutChar = 1,
    GetChar = 2,
};

pub fn putChar(x: u8) void {
    asm volatile ("ecall"
        :
        : [a0] "{a0}" (@intFromEnum(SyscallCode.PutChar)),
          [a1] "{a1}" (x),
    );
}

fn writeFn(_: void, bytes: []const u8) error{}!usize {
    if (bytes.len == 0) return 0;

    putChar(bytes[0]);
    return 1;
}

pub const Writer = std.io.Writer(void, error{}, writeFn);
pub const writer: Writer = .{ .context = {} };
