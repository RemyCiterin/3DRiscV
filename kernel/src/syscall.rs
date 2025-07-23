use crate::task::*;

pub enum SyscallError {
    OutOfMemory,
    Remap,
    UnalignedMap,
    UnalignedUmap,
    InvalidSyscall,
}

pub enum Syscall {
    Spawn{ pc: usize },
    SendMsg{ capability: Capability },
    ReceiveMsg{ capability: Capability },
    CreatePipe,
}

pub fn parse_syscall(task: &Task) -> Result<Syscall, SyscallError> {
    let ctx = task.context.read();

    match ctx.registers.a0 {
        SYS_SPAWN => Ok(Syscall::Spawn{pc: ctx.registers.a1}),
        SYS_SEND =>
            Ok(Syscall::SendMsg{capability: Capability::from(ctx.registers.a1)}),
        SYS_RECEIVE =>
            Ok(Syscall::ReceiveMsg{capability: Capability::from(ctx.registers.a1)}),
        SYS_PIPE =>
            Ok(Syscall::CreatePipe),
        _ => {Err(SyscallError::InvalidSyscall)}

    }
}

/// Syscall to spawn a new process
pub const SYS_SPAWN: usize = 1;

/// Syscall to send a message to a channel
pub const SYS_SEND: usize = 2;

/// Syscall to receive a message from a channel
pub const SYS_RECEIVE: usize = 3;

/// Syscall code to create a pipe
pub const SYS_PIPE: usize = 4;


