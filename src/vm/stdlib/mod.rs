mod functions;
mod methods;

pub(crate) use crate::vm::{
    utils::allocate, utils::FieldFnHandler, utils::FnHandler, value::Type, value::Value,
    vm::VarPtr, vm::INBUILT_FUNCTIONS, vm::INBUILT_METHODS,
};

pub(crate) use macros::*;

pub fn init() {
    functions::init();
    methods::init();
}
