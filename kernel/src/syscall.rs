use crate::scheduler::*;
use alloc::sync::Arc;
use crate::params::*;
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

pub fn handle_syscall(scheduler: &Scheduler, task: Arc<Task>) {
    let ctx = &mut task.context.write();

    match ctx.registers.a0 {
        SYS_SPAWN => {
            if let Some(new) = task.fork() {
                let new: Arc<Task> = Arc::new(new);
                new.context.write().registers = ctx.registers.clone();
                new.context.write().registers.pc += 4;
                new.context.write().registers.a0 = 0;
                new.context.write().registers.a1 = 1;
                ctx.registers.a0 = 0;
                ctx.registers.a1 = 0;

                new.capabilities.lock().extend(task.capabilities.lock().clone());

                scheduler.push(new);
            } else {
                ctx.registers.a0 = 1;
            }
        }
        SYS_SEND => {
            let size: usize = ctx.registers.a2;
            if ctx.registers.a2 > PAGE_SIZE {
                ctx.registers.a0 = 1;
                return;
            }

            let sink = Capability::from(ctx.registers.a1);
            if let Some(_) = send(task.clone(), sink, size) {
                ctx.registers.a0 = 0;
            } else {
                ctx.registers.a0 = 1;
            }
        }
        SYS_RECEIVE => {
            let source = Capability::from(ctx.registers.a1);
            if let Some(size) = receive(task.clone(), source) {
                ctx.registers.a2 = size;
                ctx.registers.a0 = 0;
            } else {
                ctx.registers.a0 = 1;
            }
        }
        SYS_PIPE => {
            let (source, sink) = task.new_pipe();
            ctx.registers.a1 = usize::from(source);
            ctx.registers.a2 = usize::from(sink);
            ctx.registers.a0 = 0;
        }
        SYS_PUTC => {
            ctx.registers.a0 = 0;
            let x: usize = ctx.registers.a1;
            crate::printer::putc(x as u8);
        }
        SYS_YIELD => {
            ctx.registers.a0 = 0;
        }
        _ => {
            ctx.registers.a0 = 1;
        }
    }
}

/// Syscall to yield
pub const SYS_YIELD: usize = 0;

/// Syscall to spawn a new process
pub const SYS_SPAWN: usize = 1;

/// Syscall to send a message to a channel
pub const SYS_SEND: usize = 2;

/// Syscall to receive a message from a channel
pub const SYS_RECEIVE: usize = 3;

/// Syscall code to create a pipe
pub const SYS_PIPE: usize = 4;

/// Syscall code to print a character
pub const SYS_PUTC: usize = 5;

