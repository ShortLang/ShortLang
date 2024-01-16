use std::{collections::HashMap, ptr::NonNull, sync::Mutex};

use lazy_static::lazy_static;

use super::value::Value;

lazy_static! {
    pub static ref ALL_ALLOCATIONS: Mutex<HashMap<usize, usize>> = Mutex::new(HashMap::new());
}

pub fn alloc_value_ptr() -> *mut Value {
    // let ptr = unsafe { alloc(LAYOUT) as *mut Value };
    let ptr = Box::leak(Box::new(Value::Nil));

    ALL_ALLOCATIONS
        .lock()
        .unwrap()
        .insert(ptr as *mut Value as usize, 0);

    ptr
}

pub fn alloc_new_value(val: Value) -> *mut Value {
    let ptr = alloc_value_ptr();
    unsafe {
        *ptr = val;
    }
    ptr
}
pub fn memory_current() -> usize {
    ALL_ALLOCATIONS.lock().unwrap().len()
}
pub fn retain(node: NonNull<Value>) -> NonNull<Value> {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();
    *all_allocations.entry(node.as_ptr() as usize).or_insert(0) += 1;
    println!("{:?}", all_allocations);
    node
}

pub fn release(node: NonNull<Value>) -> NonNull<Value> {
    let mut all_allocations = ALL_ALLOCATIONS.lock().unwrap();
    let p = node.as_ptr();
    let entry = all_allocations.entry(p as usize).or_insert(1);
    *entry -= 1;
    dbg!("Releasing Value: ", unsafe { node.as_ref() });
    if *entry == 0 {
        all_allocations.remove(&(p as usize));
        dealloc(p)
    }
    node
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

pub fn deallocate_all() {
    for (ptr, _) in ALL_ALLOCATIONS.lock().unwrap().iter() {
        let ptr: *mut Value = *ptr as *mut usize as _;
        drop(unsafe { Box::from_raw(ptr) });
    }
}

pub fn dealloc(ptr: *mut Value) {
    drop(unsafe { Box::from_raw(ptr) });
}
