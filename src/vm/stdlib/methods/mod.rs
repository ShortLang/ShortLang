mod basic;
mod file;
mod math;

pub(crate) use super::*;

pub(crate) type Data<'a> = NonNull<Value>;
pub(crate) type Args<'a> = &'a [NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub(crate) use std::ptr::NonNull;

pub fn init() {
    basic::init();
    file::init();
    math::init();
}
