use az::SaturatingCast;
use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::*;

pub fn lcm(val: Input) -> Output {
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

pub fn fib(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),

        Value::Float(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),

        _ => ret!(err: "Expected a number"),
    }
}

pub fn gcd(val: Input) -> Output {
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

pub fn abs(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(n.abs_ref().complete())),
        Value::Float(n) => ret!(Value::Float(n.abs_ref().complete(53))),

        _ => ret!(err: "Expected a number"),
    }
}

pub fn round(val: Input) -> Output {
    let &[n, precision] = val else { unreachable!() };
    let [n, mut precision] = unsafe { [n.as_ref(), precision.as_ref()] };

    if precision.is_nil() && n.is_nil() {
        ret!(err: "Expected 1 or 2 arguments, got none");
    }

    let default_value = Value::Int(1.into());
    if precision.is_nil() {
        precision = &default_value;
    }

    match (n, precision) {
        (Value::Int(n), Value::Int(_)) => ret!(Value::Int(Integer::from(n))),

        (Value::Float(n), Value::Int(precision)) => {
            let Some(precision) = precision.to_usize() else {
                ret!(err: "Precision must be a positive integer")
            };

            ret!(Value::Float(
                Float::parse(format!("{:.1$}", n, precision))
                    .unwrap()
                    .complete(53),
            ));
        }

        _ => ret!(err: "Expected a number"),
    }
}

pub fn floor(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.floor_ref().complete(53),)),

        _ => ret!(err: "Expected a number"),
    }
}

pub fn ceil(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.ceil_ref().complete(53),)),

        _ => ret!(err: "Expected a number"),
    }
}

pub fn sqrt(val: Input) -> Output {
    let default_value = Value::Int(2.into());

    let &[value, sqrt_to] = val else {
        unreachable!()
    };

    let value = unsafe { value.as_ref() };
    let mut sqrt_to = unsafe { sqrt_to.as_ref() };

    if sqrt_to.is_nil() && value.is_nil() {
        ret!(err: "Expected 1 or 2 arguments, got none");
    }

    if sqrt_to.is_nil() {
        sqrt_to = &default_value;
    }

    let sqrt_to = match sqrt_to {
        Value::Int(i) => i.saturating_cast(),
        Value::Float(f) => f.to_u32_saturating().unwrap(),

        _ => ret!(err: "Expected a number"),
    };

    let value = match value {
        Value::Int(i) => match sqrt_to {
            2 => float!(i).sqrt(),
            3 => float!(i).cbrt(),
            _ => float!(i).root(sqrt_to),
        },

        Value::Float(f) => match sqrt_to {
            2 => f.clone().sqrt(),
            3 => f.clone().cbrt(),
            _ => f.clone().root(sqrt_to),
        },

        _ => ret!(err: "Expected a number"),
    };

    Ok(Some(allocate(Value::Float(value))))
}
