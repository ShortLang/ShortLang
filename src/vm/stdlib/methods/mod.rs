mod basic;
mod file;
mod math;

use basic::*;
use file::*;
use math::*;

pub(crate) use super::*;

pub(crate) type Data<'a> = NonNull<Value>;
pub(crate) type Args<'a> = &'a [NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub(crate) use std::ptr::NonNull;

use proc_macros::shortlang_method;

pub fn init() {
    let mut ib = INBUILT_METHODS.lock().unwrap();
    hook_method!(ib, proc_macros::get_methods!())
}
