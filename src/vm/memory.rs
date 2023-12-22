use std::{
    alloc::{alloc, dealloc, Layout},
    collections::HashMap,
    sync::Mutex,
};

use lazy_static::lazy_static;

use super::value::Value;

static LAYOUT: Layout = Layout::new::<Value>();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum GCItemState {
    White,
    Grey,
    Black,
}

lazy_static! {
    pub static ref ALL_ALLOCATIONS: Mutex<HashMap<usize, GCItemState>> = Mutex::new(HashMap::new());
}

pub fn alloc_value_ptr() -> *mut Value {
    let ptr = unsafe { alloc(LAYOUT) as *mut Value };
    ALL_ALLOCATIONS
        .lock()
        .unwrap()
        .insert(ptr as usize, GCItemState::White);
    ptr
}

pub fn alloc_new_value(val: Value) -> *mut Value {
    let ptr = alloc_value_ptr();
    unsafe {
        *ptr = val;
    }
    ptr
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn mark(node: *mut Value) {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();
    let mut grey_objects = Vec::new();

    if let Some(item) = all_allocations.get_mut(&(node as usize)) {
        if *item == GCItemState::White {
            *item = GCItemState::Grey;
            grey_objects.push(node);
        }
    }

    while let Some(g) = grey_objects.pop() {
        if let Some(item) = all_allocations.get_mut(&(g as usize)) {
            if *item == GCItemState::Grey {
                *item = GCItemState::Black;
                // We'll be needing this when we addd arrays / tuples later
                if let Some(children) = unsafe { (*g).referenced_children() } {
                    for child in children {
                        if let Some(item) = all_allocations.get_mut(&(child as usize)) {
                            if *item == GCItemState::White {
                                *item = GCItemState::Grey;
                                grey_objects.push(child);
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn sweep() {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();
    let mut to_remove = Vec::new();
    for (ptr, state) in all_allocations.iter() {
        if *state == GCItemState::White {
            unsafe {
                dealloc(*ptr as *mut u8, LAYOUT);
            }
            to_remove.push(*ptr);
        }
    }

    for ptr in to_remove {
        all_allocations.remove(&ptr);
    }

    for (_, state) in all_allocations.iter_mut() {
        *state = GCItemState::White;
    }
}
