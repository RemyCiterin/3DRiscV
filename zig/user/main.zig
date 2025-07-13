const std = @import("std");
const userlib = @import("userlib.zig");

pub export const data = "Hello world!";

pub export fn main() callconv(.C) void {
    for (data) |x| userlib.putChar(x);
}
