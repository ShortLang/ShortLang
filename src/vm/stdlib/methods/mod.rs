mod basic;
mod file;

pub(crate) use super::super::{
    utils::allocate, utils::FieldFnHandler, value::Type, value::Value, vm::VarPtr,
    vm::INBUILT_METHODS,
};

use crate::*; // macros

pub(crate) type Data<'a> = NonNull<Value>;
pub(crate) type Args<'a> = &'a [NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub(crate) use std::ptr::NonNull;

pub fn init() {
    basic::init();
    file::init();
}
