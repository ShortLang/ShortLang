mod basic;
mod math;
mod os;

use basic::*;
use math::*;
use os::*;

use proc_macros::shortlang_fn;

pub(crate) use super::*;

pub(crate) type Input<'a> = &'a [std::ptr::NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub fn init() {
    let mut ib = INBUILT_FUNCTIONS.lock().unwrap();
    hook!(ib, proc_macros::get_functions!());
}
