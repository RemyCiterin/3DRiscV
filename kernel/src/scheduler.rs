use crate::task::*;
use crate::object::*;

use alloc::collections::VecDeque;

use spinning_top::{Spinlock, RwSpinlock};

use alloc::sync::Arc;

pub struct Scheduler {
    tasks: Spinlock<VecDeque<Arc<Task>>>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            tasks: Spinlock::new(VecDeque::new()),
        }
    }

    fn find_candidate(&mut self) -> Option<Arc<Task>> {
        let tasks = &mut self.tasks.lock();

        loop {
            if let Some (task) = tasks.pop_front() {
                let state = task.state.read().clone();

                match state {
                    State::Idle => {
                        tasks.push_back(task.clone());
                        return Some(task);
                    }
                    _ => {
                        tasks.push_back(task);
                    }
                }

            }
        }
    }

}
