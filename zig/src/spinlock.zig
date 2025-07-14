locked: u32 align(4) = 0,

pub const Self = @This();

pub fn init() Self {
    return .{ .locked = 0 };
}

pub inline fn unlock(self: *Self) void {
    _ = @atomicRmw(u32, &self.locked, .Xchg, 0, .acq_rel);
}

pub inline fn tryLock(self: *Self) bool {
    const busy = @atomicRmw(u32, &self.locked, .Xchg, 1, .acq_rel);
    return busy == 0;
}

pub inline fn lock(self: *Self) void {
    while (!self.tryLock()) {}
}
