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

const config = @import("config.zig");

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

    logger.info("add new executable", .{});
    const trap_ptr = try manager.new(ptable, 0x10000000, stack_usize[0..1024]);

    const trap: u32 = @intFromPtr(trap_ptr) & ~@as(u32, 0xFFF);
    try vm.map(ptable, trap, trap, 0x4000, vm.Perms.rw());
    logger.info("new executable added", .{});

    return ptable;
}

pub const std_options = std.Options{
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

    const cycle: usize = @truncate(Clint.getTime());

    const prefix =
        "[cpu {} " ++ comptime level.asText() ++ ": " ++ @tagName(scope) ++ " at cycle {}] ";

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

pub fn handler(manager: *Manager) callconv(.C) void {
    const logger = std.log.scoped(.handler);
    const pid = manager.current;
    const tp = riscv.getTP();

    if (riscv.scause.read().INTERRUPT == 0) {
        // logger.info("cause: {}", .{riscv.scause.read()});
        //logger.info("sepc: {x}", .{manager.read(pid, .pc)});

        manager.write(pid, .pc, manager.read(pid, .pc) + 4);
        manager.syscall() catch unreachable;

        //riscv.sip.modify(.{ .SSIP = 0 });
        logger.info("exception sip.ssip: {any}", .{riscv.sip.read().SSIP});
        logger.info("timecmp: {x}", .{Clint.mtimecmp[tp]});
    } else {
        logger.info("interrupt sip.ssip: {any}", .{riscv.sip.read().SSIP});
        riscv.sip.modify(.{ .SSIP = 0 });
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
        .MachineTimer = 0,
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

    const MachineState = extern struct {
        a1: usize = undefined,
        a2: usize = undefined,
        a3: usize = undefined,
        a4: usize = undefined,
        interval: usize = config.timer_step,
        mtimecmp: *volatile u64,

        // Defined in trampoline.S
        pub extern fn machine_handler() callconv(.Naked) void;
    };

    const tp: u32 = riscv.mhartid.read();
    riscv.setTP(tp);

    var state = MachineState{ .mtimecmp = &Clint.mtimecmp[tp] };

    Clint.setNextTimerInterrupt(tp);

    riscv.mstatus.modify(.{ .MPP = 1, .MPIE = 1, .MIE = 0, .SPIE = 0 });
    riscv.mie.modify(.{ .MTIE = 1 });

    riscv.mepc.write(@intFromPtr(&supervisor_main));
    riscv.satp.write(.{ .ASID = 0, .MODE = .Bare, .PPN = 0 });

    while (tp != 0) {}

    riscv.mscratch.write(@intFromPtr(&state));
    riscv.mtvec.write(@intFromPtr(&MachineState.machine_handler));

    asm volatile ("mret");
    unreachable;
}

pub export fn supervisor_main() align(16) callconv(.C) void {
    const logger = std.log.scoped(.supervisor);
    kernel_main() catch |e| {
        logger.err("kernel error: {}", .{e});
        hang();
    };
}

pub noinline fn kernel_main() anyerror!noreturn {
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

    riscv.satp.write(.{
        .PPN = @truncate(@as(u32, @intFromPtr(ptable)) / 4096),
        .MODE = .Sv32,
        .ASID = 0,
    });
    asm volatile ("sfence.vma");

    var manager = try Manager.init(malloc);

    riscv.sstatus.modify(.{ .SPIE = 1, .SIE = 0 });
    riscv.sie.modify(.{ .SSIE = 1, .STIE = 1 });
    riscv.sstatus.modify(.{ .SPP = 0 });

    _ = try setupExectable(&manager, initial_user_program[0..]);

    while (true) {
        riscv.sstatus.modify(.{ .SPP = 0 });
        manager.run();
        handler(&manager);
    }
}
