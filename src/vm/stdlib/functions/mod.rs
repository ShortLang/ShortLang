mod basic;
mod math;
mod os;

pub(crate) use super::*;

pub(crate) type Input<'a> = &'a [std::ptr::NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub fn init() {
    basic::init();
    math::init();
    os::init();
}
