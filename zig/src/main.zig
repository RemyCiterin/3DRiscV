//! this file define the system entry when using M-mode directly

const std = @import("std");
const Allocator = std.mem.Allocator;

// Control and status registers description
const riscv = @import("riscv.zig");

// UART mmio interface
const UART = @import("print.zig");
const print = UART.putString;

// User only memory protection (ensure that no other process will access the
// data)
const Spinlock = @import("spinlock.zig");

// User only syscall interface, must not be user by the kernel
const Syscall = @import("syscall.zig");

// Process managment
const Process = @import("process.zig");
const Manager = Process.Manager;

// 640*480 screen using a 256 color palette
const Screen = @import("screen.zig");

// MMC interface to an SD-card
const SdCard = @import("sdcard.zig");

// Control over timer and software interrupts
const Clint = @import("clint.zig");

// User-mode performance benchmarks
const Bench = @import("bench.zig");

const Alloc = @import("malloc.zig");

// Virtual memory
const vm = @import("vm.zig");
const ptable_t = vm.ptable_t;
const page_t = vm.page_t;

// Page allocator
const PAlloc = @import("page_allocator.zig").PAlloc;
const palloc = @import("page_allocator.zig");

var initial_user_program = @embedFile("user.bin");

// Allocate the first executable
pub fn setupExectable(manager: *Manager, binary: []const u8) anyerror!ptable_t {
    const logger = std.log.scoped(.kernel);
    const ptable: ptable_t = try vm.initTable();

    const stack = try palloc.alloc();

    const stack_usize: [*]usize = @ptrCast(&stack[0]);

    try vm.map(ptable, 0x20000000, @intFromPtr(stack), 0x1000, vm.Perms.urw());

    var pos: u32 = 0;
    var size: u32 = binary.len;

    while (size > 0) {
        const page = try palloc.alloc();
        std.mem.copyForwards(u8, page, binary[pos .. pos + @min(4096, size)]);
        try vm.map(ptable, 0x10000000 + pos, @intFromPtr(page), 4096, vm.Perms.urwx());
        size = if (size > 4096) size - 4096 else 0;
        pos += 4096;
    }

    for (0..10) |_| {
        const page = try palloc.alloc();
        try vm.map(ptable, 0x10000000 + pos, @intFromPtr(page), 4096, vm.Perms.urwx());
        pos += 4096;
    }

    const trampoline: u32 = @intFromPtr(&Process.run_user) & ~@as(u32, 0xFFF);
    try vm.map(ptable, trampoline, trampoline, 0x2000, vm.Perms.rx());

    const trap: u32 = @intFromPtr(&Process.trap_states) & ~@as(u32, 0xFFF);
    try vm.map(ptable, trap, trap, 0x4000, vm.Perms.rw());

    const satp: usize = @bitCast(@TypeOf(riscv.satp).Fields{
        .PPN = @truncate(@as(u64, @intFromPtr(ptable)) / 4096),
        .MODE = .Sv32,
        .ASID = 0,
    });

    logger.info("add new executable", .{});
    try manager.newWith(Process.TrapState{
        .registers = .{ .pc = 0x10000000 },
        .kernel_sp = undefined,
        .satp = satp,
    }, stack_usize[0..1024]);
    logger.info("new executable added", .{});

    return ptable;
}

pub const std_options = .{
    .log_level = .info,
    .logFn = log,
};

var logSpinlock = Spinlock{};

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
pub fn log(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    // This ressource is shared between kernel and users,
    // so we may observe a deadlock if this function is
    // interrupted AND used by the kernel
    const sie = riscv.sstatus.read().SIE;
    riscv.sstatus.modify(.{ .SIE = 0 });
    defer riscv.sstatus.modify(.{ .SIE = sie });

    logSpinlock.lock();
    defer logSpinlock.unlock();

    const id: u32 = riscv.getTP();

    _ = level;
    const cycle: usize = @truncate(Clint.getTime());

    const prefix =
        "[cpu {}: " ++ @tagName(scope) ++ " at cycle {}] ";

    UART.writer.print(prefix, .{ id, cycle }) catch unreachable;
    UART.writer.print(format, args) catch unreachable;
    UART.writer.print("\n", .{}) catch unreachable;
}

pub inline fn hang() noreturn {
    // Used to stop the execution in case of a simulation
    UART.putChar(0);
    while (true) {}
}

pub fn panic(
    message: []const u8,
    _: ?*std.builtin.StackTrace,
    _: ?usize,
) noreturn {
    std.log.err("Error: KERNEL PANIC \"{s}\"\n\n", .{message});
    hang();
}

pub export fn handler(manager: *Manager) callconv(.C) void {
    const logger = std.log.scoped(.handler);
    const pid = manager.current;

    if (riscv.scause.read().INTERRUPT == 0) {
        // logger.info("cause: {}", .{riscv.scause.read()});
        //logger.info("sepc: {x}", .{manager.read(pid, .pc)});

        manager.write(pid, .pc, manager.read(pid, .pc) + 4);
        manager.syscall() catch unreachable;
    } else if (riscv.sip.read().STIP == 1) {
        try UART.writer.print("timmer interrupt\n", .{});
        Clint.setNextTimerInterrupt();
        manager.next();
        logger.info("x", .{});
    } else {
        riscv.sip.modify(.{ .SEIP = 0 });
    }
}

pub export fn machine_main() align(16) callconv(.C) noreturn {
    // Deleg all interrupts
    riscv.mideleg.modify(.{
        .UserSoftware = 1,
        .UserTimer = 1,
        .UserExternal = 1,
        .SupervisorTimer = 1,
        .SupervisorSoftware = 1,
        .SupervisorExternal = 1,
        .MachineTimer = 1,
        .MachineSoftware = 1,
        .MachineExternal = 1,
    });

    // Deleg all exceptions
    riscv.medeleg.modify(.{
        .InstructionAdressMisaligned = 1,
        .InstructionAccessFault = 1,
        .IllegalInstruction = 1,
        .Breakpoint = 1,
        .LoadAdressMisaligned = 1,
        .LoadAccessFault = 1,
        .StoreAMOadessMisaligned = 1,
        .StoreAMOaccessFault = 1,
        .EnvironmentCallUmode = 1,
        .EnvironmentCallSmode = 1,
        .EnvironmentCallMmode = 1,
        .InstructionPageFault = 1,
        .LoadPageFault = 1,
        .StoreAMOpageFault = 1,
    });

    riscv.mstatus.modify(.{ .MPP = 1 });

    riscv.mepc.write(@intFromPtr(&supervisor_main));

    const tp: u32 = asm volatile ("csrr %[x], mhartid"
        : [x] "=r" (-> u32),
    );

    asm volatile ("move tp, %[tp]"
        :
        : [tp] "r" (tp),
    );

    while (tp != 0) {}

    asm volatile ("mret");
    while (true) {}
}

pub export fn supervisor_main() align(16) callconv(.C) void {
    const logger = std.log.scoped(.supervisor);
    kernel_main() catch |e| {
        logger.err("kernel error: {}", .{e});
        hang();
    };
}

pub fn kernel_main() anyerror!noreturn {
    const logger = std.log.scoped(.kernel);

    logger.info("=== Start DOoOM ===", .{});

    logger.info(
        \\Dooom Out Of Order Machine:
        \\  DOoOM is an out of order RiscV with the goal of
        \\  runing DOOM on it!
        \\
        \\  It support the rv32i isa with machine mode,
        \\  is fine-tuned to run on the ULX3S board at
        \\  25kHz, use a 8kB 2-ways cache and has an
        \\  interface for UART, SDRAM, MMC and HDMI
    , .{});

    Alloc.init();
    const malloc = Alloc.malloc;

    palloc.init();

    const ptable = try vm.initTable();

    try vm.map(ptable, 0x10000000, 0x10000000, 0x1000, vm.Perms.rw());
    try vm.map(ptable, 0x30000000, 0x30000000, 0x10000, vm.Perms.rw());
    try vm.map(ptable, 0x80000000, 0x80000000, 64 * 1024 * 1024, vm.Perms.rwx());

    try vm.map(ptable, 0x20000000, 0x83000000, 4096, vm.Perms.rwx());

    riscv.satp.write(.{
        .PPN = @truncate(@as(u32, @intFromPtr(ptable)) / 4096),
        .MODE = .Sv32,
        .ASID = 0,
    });
    asm volatile ("sfence.vma");

    var manager = try Manager.init(malloc);

    riscv.sstatus.modify(.{ .SPIE = 1 });
    riscv.sie.modify(.{ .SEIE = 0, .STIE = 1 });
    riscv.sstatus.modify(.{ .SPP = 0 });

    Clint.setNextTimerInterrupt();

    _ = try setupExectable(&manager, initial_user_program[0..]);

    while (true) {
        riscv.sstatus.modify(.{ .SPP = 0 });
        manager.run();
        handler(&manager);
    }
}
