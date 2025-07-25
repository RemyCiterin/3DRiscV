//  use crate::pointer::{Page, PAddr, VAddr};
//  use crate::object::*;
//
//  use alloc::sync::Arc;
//  use spinning_top::Spinlock;
//  use spinning_top::RwSpinlock;
//  use crate::vm;
//  use crate::trap::Context;
//  use alloc::collections::BTreeMap;
//  use alloc::vec::Vec;
//  use core::sync::atomic::AtomicU32;
//  use core::sync::atomic::Ordering;
//  use crate::palloc;
//
//  pub struct Mapping {
//      pub id: ObjectId,
//      virt: VAddr,
//      page: Page,
//  }
//
//  impl Mapping {
//      pub fn new(virt: VAddr) -> Option<Mapping> {
//          let page = palloc::alloc()?;
//
//          Some(Mapping {
//              id: new_object_id(),
//              virt,
//              page,
//          })
//      }
//
//      pub fn page(&self) -> Page {
//          self.page
//      }
//  }
