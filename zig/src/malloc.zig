const Alloc = @import("shared_alloc.zig");
const config = @import("config.zig");
const std = @import("std");

pub extern var malloc_buffer: [config.malloc_size]u8;

// The main allocator (malloc) internally contains pointers to
// those global struct, they must be global such that the callers
// of init() can reuse those pointers
var fba: std.heap.FixedBufferAllocator = undefined;
var sa: Alloc = undefined;

pub var malloc: std.mem.Allocator = undefined;

pub fn init() void {
    const logger = std.log.scoped(.kernel);

    logger.info("malloc: {*}", .{&malloc_buffer});

    // Use malloc_buffer to create the main memory allocator
    fba = std.heap.FixedBufferAllocator.init(malloc_buffer[0..]);

    // Use protect the fixed buffer allocator using a spinlock
    sa = Alloc.init(fba.allocator());

    // Initialize the main allocator
    malloc = sa.allocator();
}
