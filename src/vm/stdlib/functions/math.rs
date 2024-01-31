use az::SaturatingCast;
use rug::integer::IsPrime;
use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::*;

pub fn init() {
    let mut ib = INBUILT_FUNCTIONS.lock().unwrap();

    add_fn![ib,
        help: "Returns the lcm (least common multiple) of two values.",
        "lcm" => [lcm, 2],

        help: "Returns the gcd (greatest common divisor) of two values.",
        "gcd" => [gcd, 2],

        help: "Returns the nth fibonacci number.",
        "fib" => [fib, 1],

        help: "Returns the nth prime number.",
        "prime" => [nth_prime, 1],

        help: "Checks whether a number is prime or not.",
        "isprime" => [is_prime, 1],

        help: "Finds the previous prime number.",
        "lprime" => [last_prime, 1],

        help: "Finds the next prime number.",
        "nprime" => [next_prime, 1],

        help: "Calculates the absolute value.",
        "abs" => [abs, 1],

        help: "Rounds up to the next integer.",
        "ceil" => [ceil, 1],

        help: "Rounds down to the previous integer.",
        "floor" => [floor, 1],

        help: "Calculates the square root of a number",
        "sqrt" => [sqrt, 1],

        help: "Calculates the nth root of a number",
        "root" => [root, 2],

        help: "Rounds to the nearest integer, rounding half-way cases away from zero.",
        "round" => [round_1, 1],

        help: "Rounds to the specified precision.",
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

fn fib(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),
        Value::Float(n) => ret!(Value::Int(Integer::fibonacci(n.saturating_cast()).into())),
        _ => ret!(err: "Expected a number"),
    }
}

fn nth_prime(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
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

fn is_prime(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
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

fn last_prime(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(n.prev_prime_ref().complete())),
        Value::Float(n) => ret!(Value::Int(
            n.to_integer().unwrap_or(Integer::from(0)).prev_prime()
        )),
        _ => ret!(err: "Expected a number"),
    }
}

fn next_prime(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(n.next_prime_ref().complete())),
        Value::Float(n) => ret!(Value::Int(
            n.to_integer().unwrap_or(Integer::from(0)).next_prime()
        )),
        _ => ret!(err: "Expected a number"),
    }
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
    ret!(Value::Int(
        n.round().to_integer().unwrap_or(Integer::from(0))
    ))
}

fn round_2(val: Input) -> Output {
    let n = unsafe { val[0].as_ref().as_float() };
    let precision = unsafe { val[1].as_ref().as_int() };
    ret!(Value::Float(
        Float::parse(format!("{:.1$}", n, precision.to_usize_wrapping()))
            .unwrap()
            .complete(53)
    ));
}

fn floor(val: Input) -> Output {
    let n = unsafe { val[0].as_ref() };
    match n {
        Value::Int(n) => ret!(Value::Int(Integer::from(n))),
        Value::Float(n) => ret!(Value::Float(n.floor_ref().complete(53))),

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
