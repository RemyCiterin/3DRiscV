const Syscall = @import("syscall.zig");
const config = @import("config.zig");
const riscv = @import("riscv.zig");
const std = @import("std");
const vm = @import("vm.zig");
const ptable_t = vm.ptable_t;

const logger = std.log.scoped(.process);

pub const Registers = extern struct {
    ra: usize = 0, // x1
    sp: usize = 0, // x2
    gp: usize = 0, // x3
    tp: usize = 0, // x4
    t0: usize = 0, // x5
    t1: usize = 0, // x6
    t2: usize = 0, // x7
    s0: usize = 0, // x8
    s1: usize = 0, // x9
    a0: usize = 0, // x10
    a1: usize = 0, // x11
    a2: usize = 0, // x12
    a3: usize = 0, // x13
    a4: usize = 0, // x14
    a5: usize = 0, // x15
    a6: usize = 0, // x16
    a7: usize = 0, // x17
    s2: usize = 0, // x18
    s3: usize = 0, // x19
    s4: usize = 0, // x20
    s5: usize = 0, // x21
    s6: usize = 0, // x22
    s7: usize = 0, // x23
    s8: usize = 0, // x24
    s9: usize = 0, // x25
    s10: usize = 0, // x26
    s11: usize = 0, // x27
    t3: usize = 0, // x28
    t4: usize = 0, // x29
    t5: usize = 0, // x30
    t6: usize = 0, // x31
    pc: usize, // program counter
};

/// state saved at each interrupt, unique per CPU core
pub const TrapState = extern struct {
    /// register-set of the last interrupted user process of the CPU core
    registers: Registers,
    /// the stack pointer of the kernel idle of the CPU core
    kernel_sp: usize,
    // satp of the last user
    satp: usize = 0,
};

pub extern fn user_trap() callconv(.Naked) void;
pub extern fn run_user(*TrapState) callconv(.C) void;

pub export var trap_states: [config.max_cpus]TrapState = undefined;

pub const Process = struct {
    state: *TrapState,
    stack: []usize,
    ptable: ptable_t,
};

pub fn call_user(state: *TrapState) void {
    const cpuid = riscv.getTP();
    const trap_state = &trap_states[cpuid];

    trap_state.* = state.*;
    run_user(trap_state);
    state.* = trap_state.*;
}

// Process manager
pub const Manager = struct {
    allocator: std.mem.Allocator,

    processes: std.ArrayListUnmanaged(Process),
    current: usize,

    const Register = std.meta.FieldEnum(Registers);

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) anyerror!Self {
        const buffer = try std.ArrayListUnmanaged(Process).initCapacity(allocator, 2);
        riscv.stvec.write(@intFromPtr(&user_trap));

        return .{
            .allocator = allocator,
            .processes = buffer,
            .current = 0,
        };
    }

    pub fn run(self: *Self) void {
        run_user(self.processes.items[self.current].state);
    }

    pub fn read(self: *Self, pid: usize, comptime reg: Register) usize {
        return @field(self.processes.items[pid].state.registers, @tagName(reg));
    }

    pub fn write(self: *Self, pid: usize, comptime reg: Register, val: usize) void {
        @field(self.processes.items[pid].state.registers, @tagName(reg)) = val;
    }

    // Return a pointer to it's trap state, such that the caller can add this
    // into the page table of the process
    pub fn new(self: *Self, ptable: ptable_t, pc: usize, stack: []usize) !*TrapState {
        const satp: usize = @bitCast(@TypeOf(riscv.satp).Fields{
            .PPN = @truncate(@as(u64, @intFromPtr(ptable)) / 4096),
            .MODE = .Sv32,
            .ASID = 0,
        });

        const process = Process{
            .state = try self.allocator.create(TrapState),
            .ptable = ptable,
            .stack = stack,
        };

        process.state.* = .{
            .registers = .{ .pc = pc },
            .kernel_sp = undefined,
            .satp = satp,
        };

        try self.processes.append(self.allocator, process);

        return process.state;
    }

    pub fn next(self: *Self) void {
        self.current += 1;
        if (self.current >= self.processes.items.len)
            self.current = 0;
    }

    pub fn getSyscallCode(self: *Self, pid: usize) usize {
        return self.read(pid, .a0);
    }

    pub fn setOutput(self: *Self, pid: usize, output: Syscall.Output) void {
        const ptr: *volatile Syscall.Output = @ptrFromInt(self.read(pid, .a1));
        ptr.* = output;
    }

    pub fn getInput(self: *Self, pid: usize) Syscall.Input {
        const input: *volatile Syscall.Input = @ptrFromInt(self.read(pid, .a2));
        return input.*;
    }

    //pub inline fn exec(self: *Self, pid: usize, params: anytype) !void {
    //    const new_pid = try self.new(params.pc, params.stack_size, params.args, 0);
    //    self.setOutput(pid, .exec);
    //    self.current = new_pid;

    //    logger.info(
    //        "exec syscall from {x} to {x}",
    //        .{ self.read(pid, .pc), params.pc },
    //    );
    //}

    pub inline fn yield(self: *Self, pid: usize) void {
        logger.info("yield syscall from {x}", .{self.read(pid, .pc)});
        self.setOutput(pid, .yield);
        self.next();
    }

    pub inline fn print(self: *Self, pid: usize) void {
        @import("print.zig").putChar(@truncate(self.read(pid, .a1)));
    }

    pub inline fn syscall(self: *Self) !void {
        const pid = self.current;

        if (self.getSyscallCode(pid) == 1)
            return self.print(pid);

        switch (self.getInput(pid)) {
            .exec => {}, //try self.exec(pid, params),
            .yield => self.yield(pid),
        }
    }
};
