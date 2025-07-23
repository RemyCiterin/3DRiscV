use core::fmt::{self, Write};

use core::arch::asm;

struct Writer;

impl Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.bytes() {
            //while unsafe{*UART_LSR & UART_LSR_EMPTY_MASK} == 0 {}
            let code: usize = 0;
            unsafe { asm!("ecall", in("a0") code, in("a1") c); }
        }

        Ok(())
    }
}

pub fn print(args: fmt::Arguments) {
    Writer.write_fmt(args).unwrap();
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::printer::print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}
