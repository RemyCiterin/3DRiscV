use riscv::register::scause;
use riscv::register::sepc;

use crate::trap::Context;

pub extern "C" fn handler(state: &mut Context) {
    let scause = scause::read();

    match scause.cause() {
        scause::Trap::Exception(exception) => {
            println!("scause: exception at {:x} {:?}", sepc::read(), exception);
            state.registers.pc += 4;
        }
        scause::Trap::Interrupt(_) => {
            println!("scause: interrupt");
            // TODO
        }
    }
}
