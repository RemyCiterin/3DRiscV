const std = @import("std");
const Target = std.Target;
const Feature = Target.riscv.Feature;
const config = @import("src/config.zig").config;

const XLEN = 32;

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    const target = std.Target.Query{
        .cpu_arch = Target.Cpu.Arch.riscv32,
        .cpu_model = .{ .explicit = &Target.riscv.cpu.generic_rv32 },
        .cpu_features_add = Target.riscv.featureSet(&[_]Feature{
            .zicbom,
            .a,
            //.m,
        }),
        .os_tag = .freestanding,
        .abi = .none, // .eabi
    };

    const optimize = b.standardOptimizeOption(.{});

    //const optimizeSmall = b.standardOptimizeOption(.{
    //    .preferred_optimize_mode = .ReleaseSmall,
    //}); // .ReleaseSmall;

    const exe = b.addExecutable(.{
        .name = "kernel.elf",
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = b.resolveTargetQuery(target),
        .optimize = optimize,
    });

    const user = b.addExecutable(.{
        .name = "user.elf",
        .root_source_file = .{ .cwd_relative = "user/main.zig" },
        .target = b.resolveTargetQuery(target),
        .optimize = optimize,
    });

    //const bootloader = b.addExecutable(.{
    //    .name = "bootloader.elf",
    //    .root_source_file = .{ .cwd_relative = "src/boot.zig" },
    //    .target = b.resolveTargetQuery(target),
    //    .optimize = optimizeSmall,
    //});

    exe.addAssemblyFile(.{ .cwd_relative = "src/trampoline.S" });
    exe.addAssemblyFile(.{ .cwd_relative = "src/init.S" });
    user.addAssemblyFile(.{ .cwd_relative = "user/init.S" });

    //bootloader.addAssemblyFile(.{ .cwd_relative = "src/trampoline.s" });
    //bootloader.addAssemblyFile(.{ .cwd_relative = "src/init.S" });

    user.setLinkerScript(.{ .cwd_relative = "user/linker.ld" });
    exe.setLinkerScript(.{ .cwd_relative = "src/linker.ld" });
    //bootloader.setLinkerScriptPath(.{ .cwd_relative = "src/linker_boot.ld" });

    b.installArtifact(exe);
    b.installArtifact(user);
    //b.installArtifact(bootloader);
}
