use std::ptr::NonNull;
use std::{collections::HashMap, ops::Range};

use super::memory::alloc_new_value;
use super::value::Value;

use super::vm::{VarId, VarPtr};

#[derive(Debug, Clone)]
pub(crate) struct FunctionData {
    pub name: String,
    pub parameters: Vec<(String, VarId)>,
    pub instruction_range: Range<usize>,
    pub scope_idx: usize,
    pub returns: bool,
}

impl FunctionData {
    pub fn get_var_names(&self) -> Vec<&str> {
        self.parameters
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
    }

    pub fn get_var_ids(&self) -> Vec<VarId> {
        self.parameters
            .iter()
            .map(|(_, id)| *id)
            .collect::<Vec<_>>()
    }
}

pub fn allocate(val: Value) -> NonNull<Value> {
    NonNull::new(alloc_new_value(val)).expect("Failed to allocate")
}

#[derive(Debug, Clone)]
pub(crate) struct FnStackData {
    pub(crate) pc_before: usize,
    pub(crate) scope_idx: usize,
    pub(crate) previous_stack_len: usize,
    pub(crate) variables_id: HashMap<String, u32>,
    pub(crate) variables: HashMap<u32, Option<NonNull<Value>>>,
}

type Output = Result<VarPtr, String>;

pub struct FnHandler {
    func: Box<dyn Fn(&[NonNull<Value>]) -> Output>,
}

impl FnHandler {
    pub fn new<F: Fn(&[NonNull<Value>]) -> Output + 'static>(func: F) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(&self, params: &[NonNull<Value>]) -> Output {
        self.func.as_ref()(params)
    }
}

pub struct FieldFnHandler {
    func: Box<dyn Fn(NonNull<Value>, &[NonNull<Value>]) -> Output>,
}

impl FieldFnHandler {
    pub fn new<F: Fn(NonNull<Value>, &[NonNull<Value>]) -> Output + 'static>(func: F) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(&self, data: NonNull<Value>, params: &[NonNull<Value>]) -> Output {
        self.func.as_ref()(data, params)
    }
}


// SAFETY: We are not doing any multi-threading so it is fine
unsafe impl Send for FnHandler {}
unsafe impl Sync for FnHandler {}

unsafe impl Send for FieldFnHandler {}
unsafe impl Sync for FieldFnHandler {}

