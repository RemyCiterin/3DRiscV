locked: u32 = 0,

pub const Self = @This();

pub fn unlock(self: *Self) void {
    _ = @atomicRmw(u32, &self.locked, .Xchg, 0, .acq_rel);
}

pub fn tryLock(self: *Self) bool {
    const busy = @atomicRmw(u32, &self.locked, .Xchg, 1, .acq_rel);
    return busy == 0;
}

pub fn lock(self: *Self) void {
    while (!self.tryLock()) {}
}
