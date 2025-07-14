pub const max_cpus = 32;

pub const malloc_size = 32 * 1024 * 1024;
pub const palloc_size = 16 * 1024 * 1024;

pub const timer_step = 200000;
pub const clint_base = 0x30000000;
pub const screen_base = 0x40000000;
pub const sdcard_base = 0x50000000;

// Start address of the kernel
pub const kernel_base = 0x80010000;

// Start address of the bootloader
pub const bootloader_base = 0x80000000;
