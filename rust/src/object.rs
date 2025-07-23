use core::sync::atomic::AtomicU32;
use core::sync::atomic::Ordering;
use downcast_rs::DowncastSync;
use downcast_rs::impl_downcast;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectId(u32);

static OBJECT_ID_COUNTER: AtomicU32 = AtomicU32::new(1);

pub const KERNEL_ID: ObjectId = ObjectId(0);

pub fn new_object_id() -> ObjectId {
    ObjectId(OBJECT_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub trait Object : DowncastSync {
    fn id(&self) -> ObjectId;
}

impl_downcast!(sync Object);
