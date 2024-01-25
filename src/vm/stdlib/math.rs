use az::SaturatingCast;
use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::*;

pub fn init() {
    let mut ib = INBUILT_FUNCTIONS.lock().unwrap();

    add_fn![ib,
        "lcm" => [lcm, 2],
        "gcd" => [gcd, 2],
        "fib" => [fib, 1],
        "abs" => [abs, 1],
        "ceil" => [ceil, 1],
        "floor" => [floor, 1],

        "sqrt" => [sqrt, 1],
        "root" => [root, 2],

        "round" => [round_1, 1],
        "round" => [round_2, 2],
    ];
}

fn lcm(val: Input) -> Output {
    let &[a, b] = val else { unreachable!() };
    let [a, b] = unsafe { [a.as_ref(), b.as_ref()] };

    let lcm = match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.lcm_ref(b).complete(),
        (Value::Int(a), Value::Float(b)) => a
            .lcm_ref(&b.to_integer().unwrap_or(Integer::from(0)))
            .complete(),
        (Value::Float(a), Value::Int(b)) => a.to_integer().unwrap_or(Integer::from(0)).lcm(b),
        (Value::Float(a), Value::Float(b)) => a
            .to_integer()
            .unwrap_or(Integer::from(0))
            .lcm(&b.to_integer().unwrap_or(Integer::from(0))),

        _ => ret!(err: "Expected a number"),
    };

    ret!(Value::Int(lcm));
}

fn fib(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),
        Value::Float(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),
        _ => ret!(err: "Expected a number"),
    }
}

fn gcd(val: Input) -> Output {
    let &[a, b] = val else { unreachable!() };
    let [a, b] = unsafe { [a.as_ref(), b.as_ref()] };

    let gcd = match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.gcd_ref(b).complete(),
        (Value::Int(a), Value::Float(b)) => a
            .gcd_ref(&b.to_integer().unwrap_or(Integer::from(0)))
            .complete(),
        (Value::Float(a), Value::Int(b)) => a.to_integer().unwrap_or(Integer::from(0)).gcd(b),
        (Value::Float(a), Value::Float(b)) => a
            .to_integer()
            .unwrap_or(Integer::from(0))
            .gcd(&b.to_integer().unwrap_or(Integer::from(0))),

        _ => ret!(err: "Expected a number"),
    };

    ret!(Value::Int(gcd))
}

fn abs(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(n.abs_ref().complete())),
        Value::Float(n) => ret!(Value::Float(n.abs_ref().complete(53))),
        _ => ret!(err: "Expected a number"),
    }
}

/// Takes 1 parameter
fn round_1(val: Input) -> Output {
    let n = unsafe { val[0].as_ref().as_float() };

    ret!(Value::Float(
        Float::parse(format!("{:.1}", n)).unwrap().complete(53),
    ));
}

fn round_2(val: Input) -> Output {
    let n = unsafe { val[0].as_ref().as_float() };
    let precision = unsafe { val[1].as_ref().as_int() };

    ret!(Value::Float(
        Float::parse(format!("{:.1$}", n, precision.to_usize_wrapping()))
            .unwrap()
            .complete(53),
    ));
}

fn floor(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.floor_ref().complete(53),)),

        _ => ret!(err: "Expected a number"),
    }
}

fn ceil(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.ceil_ref().complete(53))),
        _ => ret!(err: "Expected a number"),
    }
}

fn sqrt(val: Input) -> Output {
    let n = float!(unsafe { val[0].as_ref().as_int() });
    Ok(Some(allocate(Value::Float(n.sqrt()))))
}

fn root(val: Input) -> Output {
    let [n, th_root] = unsafe { [val[0].as_ref().as_int(), val[1].as_ref().as_int()] };
    let root = float!(n).root(th_root.saturating_cast());
    Ok(Some(allocate(Value::Float(root))))
}
