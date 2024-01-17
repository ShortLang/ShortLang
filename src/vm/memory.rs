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
    let entry = all_allocations.entry(p as usize).or_insert(1);

    *entry = entry.saturating_sub(1);

    if *entry == 0 {
        // println!("marked Value: {}", unsafe { &*p });
        all_allocations.remove(&(p as usize));
        FREE_BUFFER.lock().unwrap().insert(p as usize);
    }

    node
}

pub fn free_marked() {
    let v = std::mem::take(&mut *FREE_BUFFER.lock().unwrap());

    for ptr in v.into_iter() {
        let ptr = ptr as *mut Value;
        // println!("Releasing Value: {}", unsafe { &*ptr });

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

pub fn refcount_of(ptr: NonNull<Value>) -> usize {
    *ALL_ALLOCATIONS
        .lock()
        .unwrap()
        .get(&(ptr.as_ptr() as usize))
        .unwrap()
}

pub fn deallocate_all() {
    for (ptr, _ref_count) in ALL_ALLOCATIONS.lock().unwrap().iter() {
        let ptr: *mut Value = *ptr as *mut usize as _;
        dealloc(ptr);
    }
}

#[inline(always)]
fn dealloc(ptr: *mut Value) {
    drop(unsafe { Box::from_raw(ptr) });
}
