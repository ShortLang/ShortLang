use std::collections::HashMap;
use std::collections::HashSet;
use std::ptr::NonNull;
use std::sync::Mutex;

use lazy_static::lazy_static;

use super::value::Value;

lazy_static! {
    pub static ref ALL_ALLOCATIONS: Mutex<HashMap<usize, usize>> = Mutex::new(HashMap::new());
    pub static ref FREE_BUFFER: Mutex<HashSet<usize>> = Mutex::new(HashSet::new());
}

fn alloc_value_ptr() -> *mut Value {
    let ptr = Box::leak(Box::new(Value::Nil));

    ALL_ALLOCATIONS
        .lock()
        .unwrap()
        .insert(ptr as *mut Value as usize, 0);

    ptr
}

pub fn alloc_new_value(val: Value) -> *mut Value {
    let ptr = alloc_value_ptr();
    unsafe { *ptr = val };
    ptr
}

pub fn memory_current() -> usize {
    ALL_ALLOCATIONS.lock().unwrap().len()
}

pub fn retain(node: NonNull<Value>) -> NonNull<Value> {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();

    let entry = if let Some(ptr) = all_allocations.get_mut(&(node.as_ptr() as usize)) {
        ptr
    } else if FREE_BUFFER
        .lock()
        .unwrap()
        .remove(&(node.as_ptr() as usize))
    {
        all_allocations.entry(node.as_ptr() as usize).or_insert(0)
    } else {
        return node;
    };

    *entry += 1;

    node
}

/// Decrease the reference count of the pointer
///
/// If the reference count becomes 0, the pointer
/// will be marked for deallocation, but will not be
/// deallocated before calling `free_marked`
pub fn release(node: NonNull<Value>) -> NonNull<Value> {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();
    let p = node.as_ptr();

    let refcount = all_allocations.get_mut(&(p as usize));

    if refcount.is_none() {
        return node;
    }

    let refcount = refcount.unwrap();
    *refcount = refcount.saturating_sub(1);

    if *refcount == 0 {
        all_allocations.remove(&(p as usize));
        FREE_BUFFER.lock().unwrap().insert(p as usize);
    }

    node
}

pub fn retain_multiple<const N: usize>(nodes: &[NonNull<Value>; N]) -> &[NonNull<Value>; N] {
    nodes.iter().for_each(|i| {
        let _ = retain(*i);
    });

    nodes
}

pub fn release_multiple<const N: usize>(nodes: &[NonNull<Value>; N]) -> &[NonNull<Value>; N] {
    nodes.iter().for_each(|i| {
        let _ = release(*i);
    });

    nodes
}

pub fn free_marked() {
    let v = std::mem::take(&mut *FREE_BUFFER.lock().unwrap());

    for ptr in v.into_iter() {
        let ptr = ptr as *mut Value;

        dealloc(ptr);
    }
}

pub fn release_scope(start: usize) {
    let mut allocations = ALL_ALLOCATIONS.lock().unwrap();
    for (key, value) in allocations.iter_mut().skip(start) {
        *value -= 1;
        if *value == 0 {
            ALL_ALLOCATIONS.lock().unwrap().remove(key);
            dealloc(*key as *mut Value)
        }
    }
}

pub fn refcount_of(ptr: NonNull<Value>) -> isize {
    ALL_ALLOCATIONS
        .lock()
        .unwrap()
        .get(&(ptr.as_ptr() as usize))
        .map(|i| *i as isize)
        .unwrap_or(-1)
}

pub fn deallocate_all() {
    for (ptr, _ref_count) in ALL_ALLOCATIONS.lock().unwrap().iter() {
        let ptr: *mut Value = *ptr as *mut usize as _;
        dealloc(ptr);
    }

    ALL_ALLOCATIONS.lock().unwrap().clear();
}

#[inline(always)]
fn dealloc(ptr: *mut Value) {
    drop(unsafe { Box::from_raw(ptr) });
}
