const std = @import("std");
const userlib = @import("userlib.zig");
const print = userlib.writer.print;
const Allocator = std.mem.Allocator;

pub export const data = "Hello world!";
// pub extern var malloc_buffer: [1024]u8;

//  pub const std_options = std.Options{
//      .log_level = .info,
//      .logFn = log,
//  };

// Log informations provided by the kernel. As example:
// ```zig
//  const logger = std.log.scoped(.my_scope);
//
//  pub fn foo() void {
//      // visible at runtime
//      logger.info("run foo {}", .{42});
//  }
//
//  pub fn bar() void {
//      // visible only in debug mode
//      logger.debug("run bar", .{});
//  }
// ```
// pub fn log(
//     comptime level: std.log.Level,
//     comptime scope: @TypeOf(.EnumLiteral),
//     comptime format: []const u8,
//     args: anytype,
// ) void {
//     const prefix =
//         "[" ++ comptime level.asText() ++ ": " ++ @tagName(scope) ++ "] ";
//
//     userlib.writer.print(prefix, .{}) catch unreachable;
//     userlib.writer.print(format, args) catch unreachable;
//     userlib.writer.print("\n", .{}) catch unreachable;
// }

pub export fn main() callconv(.C) void {
    //const logger = std.log.scoped(.main);

    //var fba = std.heap.FixedBufferAllocator.init(malloc_buffer[0..]);
    //const allocator = fba.allocator();

    //const y = allocator.create(u32) catch unreachable;
    //defer allocator.destroy(y);

    for (data) |x| {
        for (0..1000) |_|
            asm volatile ("nop");

        userlib.putChar(x);
    }

    //logger.info("Ah!", .{});
}
