//! Kernel use two memory allocators:
//! - A page allocator used for pages of 4096 bytes only, it is used to
//!   allocate pages, mostly to map them to user memory
//! - A second general purpose allocator used to store all the unbounded kernel
//!   data structures: vectors, trees, linked lists...
//!
//! This file implement the first one

const std = @import("std");
const Lock = @import("spinlock.zig");
const riscv = @import("riscv.zig");

var palloc: PAlloc = undefined;
pub const page_t = riscv.page_t;

pub fn init(begin: usize, end: usize) void {
    palloc = PAlloc.init(begin, end);
}

pub fn alloc() std.mem.Allocator.Error!page_t {
    return palloc.alloc();
}

pub fn free(ptr: page_t) void {
    palloc.free(ptr);
}

pub const PAlloc = struct {
    pub const Node = struct {
        next: ?*align(4096) Node,
    };

    comptime {
        if (@sizeOf(Node) >= 4096)
            @compileError("4096 must be greater than `@sizeOf(Node)`");
    }

    /// free list to allocate new memory
    node: ?*align(4096) Node = null,

    /// begin of the memory the allocator can use
    begin: usize,

    /// end of the memory the allocator can use
    end: usize,

    /// spinlock to ensure multiple threads cannot use the same resources
    lock: Lock,

    /// total number of allocations since the begining of the program
    number_alloc: usize = 0,

    /// init the kernel page allocator in a given range
    pub fn init(begin: usize, end: usize) @This() {
        var this: @This() = .{
            .lock = Lock.init(),
            .begin = begin,
            .end = end,
        };

        // free all the memory
        this.freeRange(begin, end);

        return this;
    }

    /// free all the memory strictly include between begin and end
    pub fn freeRange(this: *@This(), begin: usize, end: usize) void {
        var index: usize = begin;

        // index must be aligned on 4096 and `index > begin`
        const rest = index % 4096;
        if (rest != 0) index += 4096 - rest;

        while (index + 4096 < end) : (index += 4096) {
            this.free(@ptrFromInt(index));
        }
    }

    /// free a page, except the pointer aligned on 4096
    pub fn free(this: *@This(), ptr: page_t) void {
        const x: u64 = @intFromPtr(ptr);
        if (x % 4096 != 0) unreachable;
        if (x < this.begin) unreachable;
        if (x >= this.end) unreachable;

        this.lock.acquire();
        defer this.lock.release();

        var node: *align(4096) Node = @ptrCast(ptr);
        node.next = this.node;
        this.node = node;
    }

    /// allocate a new page and return an address aligned on 4096
    pub fn alloc(this: *@This()) std.mem.Allocator.Error!page_t {
        this.lock.acquire();
        defer this.lock.release();

        this.number_alloc += 1;

        if (this.node) |node| {
            this.node = node.next;

            return @ptrCast(node);
        }

        return error.OutOfMemory;
    }
};
