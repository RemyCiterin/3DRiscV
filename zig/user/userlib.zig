const std = @import("std");

pub fn putChar(x: u8) void {
    asm volatile ("ecall"
        :
        : [a0] "{a0}" (1),
          [a1] "{a1}" (x),
    );
}
