//! Implement Sv32 Virtual memory

const std = @import("std");
const Allocator = std.mem.Allocator;
const palloc = @import("page_allocator.zig");

const logger = std.log.scoped(.vm);

pub const VirtAddr = packed struct(u32) {
    offset: u12,
    vpn0: u10,
    vpn1: u10,

    pub fn vpn(x: u32, comptime level: comptime_int) u10 {
        const va: @This() = @bitCast(x);
        switch (level) {
            0 => return va.vpn0,
            1 => return va.vpn1,
            else => @compileError("invalid page table level"),
        }
    }
};

pub const Perms = struct {
    U: bool = false,
    X: bool = false,
    R: bool = false,
    W: bool = false,

    pub fn rwx() Perms {
        return .{ .X = true, .R = true, .W = true };
    }

    pub fn rw() Perms {
        return .{ .R = true, .W = true };
    }

    pub fn rx() Perms {
        return .{ .R = true, .X = true };
    }

    pub fn r() Perms {
        return .{ .R = true };
    }

    pub fn urwx() Perms {
        return .{ .U = true, .X = true, .R = true, .W = true };
    }

    pub fn urw() Perms {
        return .{ .U = true, .R = true, .W = true };
    }

    pub fn urx() Perms {
        return .{ .U = true, .R = true, .X = true };
    }

    pub fn ur() Perms {
        return .{ .U = true, .R = true };
    }
};

/// Definition of a page table entry
pub const pte_t = packed struct(u32) {
    V: u1 = 0,
    R: u1 = 0,
    W: u1 = 0,
    X: u1 = 0,
    U: u1 = 0,
    G: u1 = 0,
    A: u1 = 0,
    D: u1 = 0,
    RSW: u2 = 0,
    PPN: u20 = 0,
    // This implementation suppose only 32 bits of physical memory, not 34
    _reserved: u2 = 0,

    // Interpret an address as a pointer
    pub fn asPtr(this: pte_t) page_t {
        const ppn: u32 = @intCast(this.PPN);
        return @ptrFromInt(4096 * ppn);
    }

    pub fn fromPtr(ptr: page_t) pte_t {
        const addr: u32 = @intFromPtr(ptr);
        return .{ .PPN = @truncate(addr / 4096) };
    }

    // Interpret a page table entry as a page table entry
    pub fn as_ptable(this: pte_t) ptable_t {
        return @ptrCast(this.asPtr());
    }

    pub fn fromptable_t(ptr: ptable_t) pte_t {
        const addr: u32 = @intFromPtr(ptr);
        return .{ .PPN = @truncate(addr / 4096) };
    }

    pub fn valid(self: pte_t) bool {
        return self.V == 1;
    }

    pub fn leaf(self: pte_t) bool {
        return self.R != 0 or self.W != 0 or self.X != 0;
    }

    pub fn setPerms(this: *pte_t, p: Perms) void {
        this.R = if (p.R) 1 else 0;
        this.W = if (p.W) 1 else 0;
        this.X = if (p.X) 1 else 0;
        this.U = if (p.U) 1 else 0;
    }

    pub fn perms(this: pte_t) Perms {
        return .{
            .R = this.R == 1,
            .W = this.W == 1,
            .X = this.X == 1,
            .U = this.U == 1,
        };
    }

    pub fn global(this: pte_t) bool {
        return this.G == 1;
    }
};

// A page table is just an aligned array of 512 entries
pub const ptable_t = *align(4096) [1024]pte_t;
pub const page_t = palloc.page_t;

pub fn initTable() anyerror!ptable_t {
    const page = palloc.alloc() catch |err| {
        logger.info("initTable: kernel page allocation fail", .{});
        //@panic("vm.initTable");
        return err;
    };

    const table: ptable_t = @ptrCast(page);
    @memset(table, .{});
    return table;
}

pub fn walkAndAlloc(table: ptable_t, va: u32) anyerror!*pte_t {
    comptime var level: isize = 1;
    var ptable: ptable_t = table;

    inline while (true) : (level -= 1) {
        const entry: *pte_t = &ptable.*[VirtAddr.vpn(va, level)];

        if (level == 0)
            return entry;

        if (entry.valid()) {
            ptable = entry.as_ptable();
        } else {
            ptable = try initTable();
            entry.* = pte_t.fromptable_t(ptable);
            entry.V = 1;
        }
    }
}

pub fn walk(table: ptable_t, va: u32) ?*pte_t {
    comptime var level: isize = 1;
    var ptable: ptable_t = table;

    inline while (true) : (level -= 1) {
        const entry: *pte_t = &ptable.*[VirtAddr.vpn(va, level)];

        if (level == 0)
            return entry;

        if (!entry.valid()) return null;
        ptable = entry.as_ptable();
    }
}

pub fn map(
    table: ptable_t,
    virt: u32,
    phys: u32,
    size: u32,
    perms: Perms,
) anyerror!void {
    var pa = phys;
    var va = virt;
    const last: u32 = va + size - 4096;

    logger.info(
        "map [0x{x}, 0x{x}] to [0x{x}, 0x{x}]",
        .{ virt, virt + size - 1, phys, phys + size - 1 },
    );

    if (va % 4096 != 0 or size % 4096 != 0 or size == 0) {
        @panic("vm.map: unaligned virtual memory map");
    }

    while (true) {
        var entry: *pte_t = try walkAndAlloc(table, va);

        if (entry.valid()) {
            logger.info("remap virtual address {x}", .{va});
            @panic("vm.map");
        }

        entry.PPN = @truncate(pa / 4096);
        entry.setPerms(perms);
        entry.V = 1;

        if (va == last) return;

        va += 4096;
        pa += 4096;
    }
}

pub fn unmap(
    table: ptable_t,
    virt: u32,
    size: u32,
    free: bool,
) void {
    var va = virt;
    const last: u32 = va + size - 4096;

    logger.info(
        "unmap [0x{x}, 0x{x}]",
        .{ virt, virt + size - 1 },
    );

    if (va % 4096 != 0 or size % 4096 != 0 or size == 0)
        return error.VirtAlign;

    while (true) : (va += 4096) {
        var entry: *pte_t = try walk(table, va) orelse {
            logger.info("virtual address {} is unmapped", .{va});
            @panic("vm.unmap");
        };

        if (!entry.valid()) {
            logger.info("virtual address {} is unmapped", .{va});
            @panic("vm.unmap");
        }

        if (free)
            palloc.free(entry.asPtr());
        entry.* = .{};

        if (va == last) return;
    }
}
