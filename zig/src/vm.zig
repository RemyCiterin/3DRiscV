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

pub const WalkOutput = struct {
    level: isize,
    pte: *pte_t,
};

pub fn walkAndAlloc(table: ptable_t, va: u32, megapage: bool) anyerror!WalkOutput {
    comptime var level: isize = 1;
    var ptable: ptable_t = table;

    inline while (true) : (level -= 1) {
        const entry: *pte_t = &ptable.*[VirtAddr.vpn(va, level)];

        if (level == 0 or entry.leaf())
            return .{ .level = level, .pte = entry };

        if (entry.valid()) {
            ptable = entry.as_ptable();
        } else if (megapage) {
            return .{ .level = 1, .pte = entry };
        } else {
            ptable = try initTable();
            entry.* = pte_t.fromptable_t(ptable);
            entry.V = 1;
        }
    }
}

pub fn walk(table: ptable_t, va: u32) WalkOutput {
    comptime var level: isize = 1;
    var ptable: ptable_t = table;

    inline while (true) : (level -= 1) {
        const entry: *pte_t = &ptable.*[VirtAddr.vpn(va, level)];

        if (level == 0 or !entry.valid() or entry.leaf())
            return .{ .level = level, .pte = entry };

        ptable = entry.as_ptable();
    }
}

pub const MapError = error{
    Unaligned,
    Remap,
};

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

    if (va % 4096 != 0 or size % 4096 != 0 or size == 0 or pa % 4096 != 0) {
        logger.info(
            "va: 0x{x}, pa: 0x{x} or size: 0x{x} is not aligned",
            .{ va, pa, size },
        );
        return MapError.Unaligned;
    }

    while (true) {
        const psize: usize = 0x1000;
        const mpsize: usize = 0x400000;
        var megapage =
            last >= va + mpsize and pa % mpsize == 0 and va % mpsize == 0;

        const output = try walkAndAlloc(table, va, megapage);
        const entry: *pte_t = output.pte;
        megapage = output.level == 1;

        if (entry.valid()) {
            logger.info("remap virtual address {x}", .{va});
            return MapError.Remap;
        }

        entry.PPN = @truncate(pa / 4096);
        entry.setPerms(perms);
        entry.V = 1;

        if (va == last) return;

        va += if (megapage) mpsize else psize;
        pa += if (megapage) mpsize else psize;
    }
}

pub const UnmapError = error{
    UnalignedMegapage,
    TooSmallMegapage,
    Unaligned,
    Unmapped,
};

// Must return without error if it match a previous map
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
        return UnmapError.Unaligned;

    while (true) {
        const psize: usize = 0x1000;
        const mpsize: usize = 0x400000;
        const output = walk(table, va);
        const entry: *pte_t = output.pte;
        const megapage = output.level == 1;

        if (!entry.valid()) {
            logger.info("virtual address 0x{x} is unmapped", .{va});
            return UnmapError.Unmapped;
        }

        if (megapage and va % mpsize != 0) {
            logger.info("virtual address 0x{x} is unaligned on a megapage", .{va});
            return UnmapError.UnalignedMegapage;
        }

        if (megapage and last < va + mpsize) {
            logger.info("size is too small for a megapage deallocation", .{});
            return UnmapError.TooSmallMegapage;
        }

        if (free)
            palloc.free(entry.asPtr());
        entry.* = .{};

        if (va == last) return;

        va += if (megapage) mpsize else psize;
    }
}
