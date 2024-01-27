mod basic;
mod macros;
mod math;
mod os;

pub(crate) use super::super::{
    utils::allocate,
    utils::FnHandler,
    value::Value,
    vm::{VarPtr, INBUILT_FUNCTIONS},
};

pub(crate) use crate::*; // macros

pub(crate) type Input<'a> = &'a [std::ptr::NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub fn init() {
    basic::init();
    math::init();
    os::init();
}
