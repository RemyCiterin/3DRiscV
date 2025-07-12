//! this file define the system entry when using M-mode directly

const std = @import("std");
const Allocator = std.mem.Allocator;

// Control and status registers description
const RV = @import("riscv.zig");

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

// Protected user memory allocator
const UserAlloc = @import("user_alloc.zig");

// Virtual memory
const VM = @import("vm.zig");

// Page allocator
const PAlloc = @import("page_allocator.zig").PAlloc;
const palloc = @import("page_allocator.zig").palloc;

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
    const sie = RV.sstatus.read().SIE;
    RV.sstatus.modify(.{ .SIE = 0 });
    defer RV.sstatus.modify(.{ .SIE = sie });

    logSpinlock.lock();
    defer logSpinlock.unlock();

    const id: u32 = RV.getTP();

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

extern var kalloc_buffer: [31 * 1024 * 1024]u8;

// Main kernel allocator
pub var kalloc: Allocator = undefined;
pub var malloc: Allocator = undefined;

pub export fn handler(manager: *Manager) callconv(.C) void {
    const logger = std.log.scoped(.handler);
    const pid = manager.current;

    if (RV.scause.read().INTERRUPT == 0) {
        logger.info("cause: {}", .{RV.scause.read()});
        logger.info("sepc: {x}", .{manager.read(pid, .pc)});

        manager.write(pid, .pc, manager.read(pid, .pc) + 4);
        manager.syscall() catch unreachable;
    } else if (RV.sip.read().STIP == 1) {
        try UART.writer.print("timmer interrupt\n", .{});
        Clint.setNextTimerInterrupt();
        manager.next();
        logger.info("x", .{});
    } else {
        RV.sip.modify(.{ .SEIP = 0 });
    }
}

pub export fn machine_main() align(16) callconv(.C) noreturn {
    // We deleg all the interrupts and exceptions so we don't need
    // an interrupt handler here

    RV.mideleg.modify(.{
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

    RV.medeleg.modify(.{
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

    RV.mstatus.modify(.{ .MPP = 1 });

    RV.mepc.write(@intFromPtr(&supervisor_main));

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
    //supervisor_main();
}

pub export fn supervisor_main() align(16) callconv(.C) void {
    kernel_main();
}

pub export fn kernel_main() align(16) callconv(.C) void {
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

    const kalloc_len = 20 * 1024 * 1024;
    var kernel_fba = std.heap.FixedBufferAllocator.init(kalloc_buffer[0..kalloc_len]);
    kalloc = kernel_fba.allocator();

    var user_fba = std.heap.FixedBufferAllocator.init(kalloc_buffer[kalloc_len..]);
    var user_alloc = UserAlloc.init(user_fba.allocator());
    malloc = user_alloc.allocator();

    var manager = Manager.init(kalloc);
    _ = manager.new(@intFromPtr(&user_main), 4096, &malloc, 0) catch unreachable;

    RV.sstatus.modify(.{ .SPIE = 1 });
    RV.sie.modify(.{ .SEIE = 0, .STIE = 1 });
    RV.sstatus.modify(.{ .SPP = 1 });

    Clint.setNextTimerInterrupt();

    while (true) {
        logger.info("run pc={}", .{manager.current});
        RV.sstatus.modify(.{ .SPP = 1 });
        manager.run();
        handler(&manager);
    }

    @panic("unreachable");
}

pub export fn user_main(pid: usize, alloc: *Allocator) callconv(.C) noreturn {
    const logger = std.log.scoped(.user);

    logger.info("process {} started", .{pid});

    //Syscall.yield();
    Syscall.exec(@intFromPtr(&user_main), 512, &malloc);

    logger.info("Binary Search:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.BinarySearch.init(alloc.*, size) catch unreachable;
        Bench.measure(size, &bench);
        bench.free();
    }

    logger.info("Fibo:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.Fibo.init(size);
        const fibo = Bench.measure(size, &bench);
        //logger.info("fibo({}) = {}", .{ size, fibo });
        _ = fibo;
    }

    logger.info("Fibo Recursive:", .{});
    for (1..11) |i| {
        const size = 2 * i;
        var bench = Bench.FiboRec.init(size);
        const fibo = Bench.measure(size, &bench);
        //logger.info("fibo({}) = {}", .{ size, fibo });
        _ = fibo;
    }

    logger.info("ALU latency:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.LatencyALU.init(size);
        const output = Bench.measure(size, &bench);
        _ = output;
    }

    logger.info("ALU bandwidth:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.BandwidthALU.init(size);
        const output = Bench.measure(size, &bench);
        _ = output;
    }

    logger.info("LSU latency:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.LatencyLSU.init(size);
        const output = Bench.measure(size, &bench);
        _ = output;
    }

    logger.info("LSU bandwidth:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.BandwidthLSU.init(size);
        const output = Bench.measure(size, &bench);
        _ = output;
    }

    logger.info("Merge Sort:", .{});
    for (1..11) |i| {
        const size = 10 * i;
        var bench = Bench.Sort.init(alloc.*, size) catch unreachable;
        Bench.measure(size, &bench);
        bench.free();
    }

    logger.info("Matrix Multiplication:", .{});
    for (1..11) |i| {
        const size = 2 * i;
        var bench = Bench.MatrixMult.init(alloc.*, size) catch unreachable;
        Bench.measure(size, &bench);
        bench.free();
    }

    const pixel = Screen.Pixel{ .blue = 0b10, .red = 0b100 };
    pixel.fillRectangle(100, 0, 100, 240 - 1);
    pixel.fillRectangle(0, 100, 320 - 1, 100);
    hang();
}
