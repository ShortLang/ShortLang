use az::SaturatingCast;
use rug::integer::IsPrime;
use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::*;

#[shortlang_fn(
    args = 2,
    help = "Returns the lcm (least common multiple) of two values."
)]
pub fn lcm(val: Input) -> Output {
    let [a, b] = [nth_arg!(val, 0), nth_arg!(val, 1)];

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

#[shortlang_fn(
    args = 2,
    help = "Returns the gcd (greatest common divisor) of two values."
)]
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

#[shortlang_fn(args = 1, help = "Returns the nth fibonacci number.")]
pub fn fib(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),
        Value::Float(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),

        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(name = "prime", args = 1, help = "Returns the nth prime number.")]
pub fn nth_prime(val: Input) -> Output {
    let n = nth_arg!(val, 0);

    match n {
        Value::Int(n) => {
            let mut current = Integer::from(1);
            for _ in 0..n.saturating_cast() {
                current.next_prime_mut();
            }
            ret!(Value::Int(current))
        }
        Value::Float(n) => {
            let mut current = Integer::from(1);
            for _ in 0..n.to_integer().unwrap_or(Integer::from(0)).saturating_cast() {
                current.next_prime_mut();
            }
            ret!(Value::Int(current))
        }
        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(
    name = "isprime",
    args = 1,
    help = "Checks whether a number is prime or not."
)]
pub fn is_prime(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => match n.is_probably_prime(30) {
            IsPrime::Yes | IsPrime::Probably => ret!(Value::Bool(true)),
            _ => ret!(Value::Bool(false)),
        },
        Value::Float(n) => {
            match n
                .to_integer()
                .unwrap_or(Integer::from(0))
                .is_probably_prime(30)
            {
                IsPrime::Yes | IsPrime::Probably => ret!(Value::Bool(true)),
                _ => ret!(Value::Bool(false)),
            }
        }
        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(name = "lprime", args = 1, help = "Finds the previous prime number.")]
pub fn last_prime(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(n.prev_prime_ref().complete())),
        Value::Float(n) => ret!(Value::Int(
            n.to_integer().unwrap_or(Integer::from(0)).prev_prime()
        )),
        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(name = "nprime", args = 1, help = "Finds the next prime number.")]
pub fn next_prime(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(n.next_prime_ref().complete())),
        Value::Float(n) => ret!(Value::Int(
            n.to_integer().unwrap_or(Integer::from(0)).next_prime()
        )),
        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(args = 1, help = "Calculates the absolute value.")]
pub fn abs(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(n.abs_ref().complete())),
        Value::Float(n) => ret!(Value::Float(n.abs_ref().complete(53))),
        _ => ret!(err: "Expected a number"),
    }
}

/// Takes 1 parameter
#[shortlang_fn(
    args = 1,
    help = "Rounds to the nearest integer, rounding half-way cases away from zero."
)]
pub fn round(val: Input) -> Output {
    let n = cast_nth_arg!(val, 0, Float);
    ret!(Value::Int(
        n.round().to_integer().unwrap_or(Integer::from(0))
    ))
}

#[shortlang_fn(name = "round", args = 2, help = "Rounds to the specified precision.")]
pub fn round_2(val: Input) -> Output {
    let n = cast_nth_arg!(val, 0, Float);
    let precision = cast_nth_arg!(val, 1, Int);

    ret!(Value::Float(
        Float::parse(format!("{:.1$}", n, precision.to_usize_wrapping()))
            .unwrap()
            .complete(53)
    ));
}

#[shortlang_fn(args = 1, help = "Rounds down to the previous integer.")]
pub fn floor(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.floor_ref().complete(53))),

        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(args = 1, help = "Rounds up to the next integer.")]
pub fn ceil(val: Input) -> Output {
    let n = nth_arg!(val, 0);
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.ceil_ref().complete(53))),
        _ => ret!(err: "Expected a number"),
    }
}

#[shortlang_fn(args = 1, help = "Calculates the square root of a number")]
pub fn sqrt(val: Input) -> Output {
    let n = float!(cast_nth_arg!(val, 0, Int));
    Ok(Some(allocate(Value::Float(n.sqrt()))))
}

#[shortlang_fn(args = 2, help = "Calculates the nth root of a number")]
pub fn root(val: Input) -> Output {
    let [n, th_root] = [cast_nth_arg!(val, 0, Int), cast_nth_arg!(val, 1, Int)];
    let root = float!(n).root(th_root.saturating_cast());
    Ok(Some(allocate(Value::Float(root))))
}
