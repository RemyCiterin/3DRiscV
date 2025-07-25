use alloc::collections::VecDeque;
use alloc::sync::{Arc, Weak};
use spinning_top::Spinlock;

use crate::object::*;

/// Message source
pub struct Source {
    /// Unique ID of the source
    pub id: ObjectId,

    /// Owner of the queue (0 for kernel)
    pub owner: ObjectId,

    /// A list of tasks waiting for their messages to be received,
    /// those tasks are blocked if they are on this list
    pub block: Spinlock<VecDeque<Arc<dyn Object>>>,

    /// The sink associated with this source
    pub sink: Option<Weak<Sink>>,
}

impl Object for Source {
    fn id(&self) -> ObjectId { self.id }
}

/// Message source
pub struct Sink{
    /// Unique ID of the source
    pub id: ObjectId,

    /// Owner of the queue (0 for kernel)
    pub owner: ObjectId,

    /// A list of tasks waiting for a message from this queue,
    /// those tasks are blocked if they are on this list
    pub block: Spinlock<VecDeque<Arc<dyn Object>>>,

    /// The source associated with this sink
    pub source: Option<Weak<Source>>,
}

impl Object for Sink {
    fn id(&self) -> ObjectId { self.id }
}

/// To ensure forward progress in the message transmissions, we must ensure that
/// at leat of the `block` queues (of the source and sink) is empty at any times
pub fn new_channel(owner: ObjectId) -> (Arc<Source>, Arc<Sink>) {
    let mut sink = Arc::new(Sink{
        block: Spinlock::new(VecDeque::new()),
        source: Some(Weak::new()),
        id: new_object_id(),
        owner
    });

    let source = Arc::new(Source{
        block: Spinlock::new(VecDeque::new()),
        sink: Some(Arc::downgrade(&sink)),
        id: new_object_id(),
        owner
    });

    unsafe {
        Arc::get_mut_unchecked(&mut sink).source = Some(Arc::downgrade(&source));
    }

    (source, sink)
}

pub fn new_source(owner: ObjectId) -> Arc<Source> {
    Arc::new(Source{
        block: Spinlock::new(VecDeque::new()),
        id: new_object_id(),
        sink: None,
        owner
    })
}

pub fn new_sink(owner: ObjectId) -> Arc<Sink> {
    Arc::new(Sink{
        block: Spinlock::new(VecDeque::new()),
        id: new_object_id(),
        source: None,
        owner
    })
}

pub enum ChannelError {
    Disconected,
}

impl Sink {
    /// Return the id of a task that wait for a message
    /// and unlock it, block the task otherwise
    pub fn send(&self, task: Arc<dyn Object>)
        -> Result<Option<Arc<dyn Object>>, ChannelError> {
        if !self.block.lock().is_empty() {
            // Another task is already waiting
            self.block.lock().push_back(task);
            return Ok(None);
        }

        if let Some(ref source) = self.source {
            match source.upgrade() {
                Some(source) => {
                    if source.block.lock().is_empty() {
                        self.block.lock().push_back(task);
                        Ok(None)
                    } else {
                        let x = source.block.lock().front().unwrap().clone();
                        source.block.lock().pop_front();
                        Ok(Some(x))
                    }
                }
                None => Err(ChannelError::Disconected)
            }
        } else {
            Ok(Some(Arc::new(KERNEL{})))
        }
    }
}

impl Source {
    /// Return the id of a task that wait for a message to be received
    /// and unlock it, block the task otherwise
    pub fn receive(&self, task: Arc<dyn Object>)
        -> Result<Option<Arc<dyn Object>>, ChannelError> {
        if !self.block.lock().is_empty() {
            // Another task is already waiting
            self.block.lock().push_back(task);
            return Ok(None);
        }

        if let Some(ref sink) = self.sink {
            match sink.upgrade() {
                Some(sink) => {
                    if sink.block.lock().is_empty() {
                        self.block.lock().push_back(task);
                        Ok(None)
                    } else {
                        let x = sink.block.lock().front().unwrap().clone();
                        sink.block.lock().pop_front();
                        Ok(Some(x))
                    }
                }
                None => Err(ChannelError::Disconected)
            }
        } else {
            Ok(Some(Arc::new(KERNEL{})))
        }
    }
}
