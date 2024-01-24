mod basic;
mod macros;
mod math;
mod utils;

pub(crate) use super::utils::allocate;
pub(crate) use super::utils::Handler;
pub(crate) use super::value::Value;
pub(crate) use super::vm::{VarPtr, INBUILT_FUNCTIONS};

pub(crate) use crate::*; // macros

pub(crate) type Input<'a> = &'a [std::ptr::NonNull<Value>];
pub(crate) type Output = Result<VarPtr, String>;

pub fn init() {
    let mut ib = INBUILT_FUNCTIONS.lock().unwrap();

    // TODO: Create separate overloaded functions
    add_fn![ib,
        "$" => [basic::println, 1],
        "$$" => [basic::print, 1],

        "inp" => [basic::input, 1],
        "chr" => [basic::char, 1],
        "ord" => [basic::ord, 1],

        "len" => [basic::len, 1],
        "exit" => [basic::exit, 0],
        "type" => [basic::get_type, 1],
        "open" => [basic::open, 1],

        "rng" => [basic::rng_1, 1],
        "rng" => [basic::rng_2, 2],

        "rnd" => [basic::rnd_0, 0],
        "rnd" => [basic::rnd_1, 1],
        "rnd" => [basic::rnd_2, 2],

        "flt" => [basic::to_float, 1],
        "str" => [basic::to_str, 1],
        "int" => [basic::to_int, 1],

        "lcm" => [math::lcm, 2],
        "gcd" => [math::gcd, 2],
        "fib" => [math::fib, 1],
        "abs" => [math::abs, 1],
        "ceil" => [math::ceil, 1],
        "floor" => [math::floor, 1],

        "sqrt" => [math::sqrt, 1],
        "root" => [math::root, 2],

        "round" => [math::round_1, 1],
        "round" => [math::round_2, 2],
    ];
}
