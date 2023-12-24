use std::ops::Range;
use std::ptr::NonNull;

use super::{memory::alloc_new_value, value::Value, vm::VarId};

#[derive(Debug, Clone)]
pub(crate) struct FunctionData {
    pub name: String,
    pub parameters: Vec<(String, VarId)>,
    pub instruction_range: Range<usize>,
    pub returns: bool,
    pub scope_idx: usize,
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
    NonNull::new(alloc_new_value(val)).expect("failed to allocate")
}
