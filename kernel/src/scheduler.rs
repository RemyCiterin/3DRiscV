use crate::task::*;
use crate::object::*;

use alloc::collections::VecDeque;
use alloc::vec::*;

use spinning_top::{Spinlock};

use alloc::sync::Arc;

pub struct Scheduler {
    tasks: Spinlock<VecDeque<Arc<Task>>>,
    blocked: Spinlock<Vec<Arc<Task>>>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            tasks: Spinlock::new(VecDeque::new()),
            blocked: Spinlock::new(Vec::new()),
        }
    }

    pub fn push(&self, task: Arc<Task>) {
        self.tasks.lock().push_back(task);
    }

    pub fn canonicalize(&self) {
        let tasks = &mut self.tasks.lock();
        let blocked = &mut self.blocked.lock();

        tasks.extend(blocked.iter().cloned());
        blocked.clear();
    }

    pub fn remove(&self, task: Arc<Task>) {
        let tasks = &mut self.tasks.lock();
        let blocked = &mut self.blocked.lock();

        for i in 0..tasks.len() {
            if tasks[i].id() == task.id() { tasks.remove(i); }
        }

        for i in 0..blocked.len() {
            if blocked[i].id() == task.id() { blocked.remove(i); }
        }
    }

    pub fn choose_task(&self) -> Option<Arc<Task>> {
        let tasks = &mut self.tasks.lock();
        let blocked = &mut self.blocked.lock();

        while let Some (task) = tasks.pop_front() {
            let state = &mut task.state.write();

            match state.clone() {
                State::Idle => {
                    state.clone_from(&State::Busy);
                    blocked.push(task.clone());
                    return Some(task.clone());
                }
                _ => blocked.push(task.clone())
            }
        }

        None
    }
}
