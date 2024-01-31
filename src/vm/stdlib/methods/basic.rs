use super::*;
use rug::Float;
use rug::Integer;
use std::ops::{AddAssign, MulAssign, SubAssign};

pub fn init() {
    let mut functions = INBUILT_METHODS.lock().unwrap();

    add_method![functions,
        help: "Pushes a value in a string or an array.",
        "push" => [push, 1, Type::String, Type::Array],

        help: "Flattens an array a single value string, placing a given separator between each element.",
        "join" => [join, 1, Type::Array],

        help: "Returns an array containing the substrings of a string, separated by specified character.",
        "split" => [split, 1, Type::String],

        help: "Calculates the sum of all the values inside an array.",
        "sum" => [sum, 0, Type::Array],

        help: "Subtracts all the elements from the first one.",
        "sub" => [sub, 0, Type::Array],

        help: "Calculates the product of all the values of the array.",
        "mul" => [mul, 0, Type::Array],

        help: "Finds the smallest element in the array.",
        "min" => [min, 0, Type::Array],

        help: "Finds the largest element in the array.",
        "max" => [max, 0, Type::Array],

        help: "Sorts the array.",
        "sort" => [sort, 0, Type::Array],

        help: "Returns and removes the last element of the array.",
        "pop" => [pop, 0, Type::String, Type::Array],

        help: "Removes all the elemements of the array.",
        "clear" => [clear, 0, Type::String, Type::Array],

        help: "Returns the type of the value.",
        "type" => [get_type, 0, Type::String, Type::Array, Type::Integer, Type::Float, Type::Nil],
    ];
}

fn push(mut data: Data, args: Args) -> Output {
    let src = unsafe { args[0].as_ref() };
    let data = unsafe { data.as_mut() };
    *data = data.binary_add(src).unwrap();
    ret!(ptr: data);
}

fn pop(mut data: Data, _: Args) -> Output {
    let val = match unsafe { data.as_mut() } {
        Value::Array(a) => a.pop(),
        Value::String(s) => s.pop().map(|i| Value::String(i.to_string())),
        _ => unreachable!(),
    };
    ret!(val.unwrap_or(Value::Nil))
}

fn clear(mut data: Data, _: Args) -> Output {
    unsafe {
        data.as_mut().clear();
    }
    ret!()
}

fn join(data: Data, args: Args) -> Output {
    let separator = unsafe { args[0].as_ref() };

    let array = unsafe { data.as_ref().as_array() };
    let result_string = array
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(match separator {
            Value::Nil => "",
            Value::String(s) => s,

            _ => ret!(err: &format!(
                "Cannot join with the value of type '{}'",
                separator.get_type()
            )),
        });

    ret!(result_string.into())
}

fn split(data: Data, args: Args) -> Output {
    let split = unsafe { args[0].as_ref() };

    let data = unsafe { data.as_ref() };

    let empty = String::new();
    let (val_str, split_str) = match (data, split) {
        (Value::String(val_str), Value::String(split_str)) => (val_str, split_str),
        (Value::Nil, Value::String(split_str)) => (split_str, &empty),
        (Value::String(val_str), Value::Nil) => (val_str, &empty),
        _ => ret!(err: &format!(
            "Expected 'str' as an argument of 'split' function, found '{}'",
            data.get_type()
        )),
    };

    let split = if split_str.is_empty() {
        val_str
            .chars()
            .map(|i| i.to_string().into())
            .collect::<Vec<Value>>()
    } else {
        val_str
            .split(split_str)
            .map(|i| i.into())
            .collect::<Vec<Value>>()
    };

    ret!(Value::Array(split))
}

fn sum(data: Data, _: Args) -> Output {
    let array = unsafe { data.as_ref().as_array() };
    let mut sum = Float::new(53);
    for i in array.iter() {
        match i {
            Value::Int(i) => sum.add_assign(i),
            Value::Float(f) => sum.add_assign(f),
            Value::String(s) => sum.add_assign(s.len()),
            Value::Array(a) => sum.add_assign(a.len()),
            _ => (),
        }
    }
    ret!(Value::Float(sum))
}

fn sub(data: Data, args: Args) -> Output {
    let array = unsafe { data.as_ref().as_array() };
    let mut sum = Float::new(53);
    for i in array.iter() {
        match i {
            Value::Int(i) => sum.sub_assign(i),
            Value::Float(f) => sum.sub_assign(f),
            Value::String(s) => sum.sub_assign(s.len()),
            Value::Array(a) => sum.sub_assign(a.len()),
            _ => (),
        }
    }
    ret!(Value::Float(sum))
}

fn mul(data: Data, args: Args) -> Output {
    let array = unsafe { data.as_ref().as_array() };
    let mut sum = Float::new(53);
    for i in array.iter() {
        match i {
            Value::Int(i) => sum.mul_assign(i),
            Value::Float(f) => sum.mul_assign(f),
            Value::String(s) => sum.mul_assign(s.len()),
            Value::Array(a) => sum.mul_assign(a.len()),
            _ => (),
        }
    }
    ret!(Value::Float(sum))
}

fn min(data: Data, args: Args) -> Output {
    let array = unsafe { data.as_ref().as_array() };
    let mut min = float!(rug::float::Special::Infinity);
    for i in array.iter() {
        match i {
            Value::Int(i) => min.min_mut(&float!(i)),
            Value::Float(f) => min.min_mut(f),
            Value::String(s) => min.min_mut(&float!(s.len())),
            Value::Array(a) => min.min_mut(&float!(a.len())),
            _ => (),
        }
    }
    ret!(Value::Float(min))
}

fn max(data: Data, args: Args) -> Output {
    let array = unsafe { data.as_ref().as_array() };
    let mut max = float!(rug::float::Special::NegInfinity);
    for i in array.iter() {
        match i {
            Value::Int(i) => max.max_mut(&float!(i)),
            Value::Float(f) => max.max_mut(f),
            Value::String(s) => max.max_mut(&float!(s.len())),
            Value::Array(a) => max.max_mut(&float!(a.len())),
            _ => (),
        }
    }
    ret!(Value::Float(max))
}

fn sort(data: Data, _: Args) -> Output {
    let mut array = unsafe { data.as_ref().as_array().to_vec() };
    array.sort_by(|a, b| match (a, b) {
        (Value::Float(f), _) if f.is_infinite() && f.is_sign_negative() => std::cmp::Ordering::Less,
        (_, Value::Float(f)) if f.is_infinite() && f.is_sign_negative() => {
            std::cmp::Ordering::Greater
        }
        (Value::Int(i), Value::Float(f)) => float!(i).partial_cmp(f).unwrap(),
        (Value::Float(f), Value::Int(i)) => f.partial_cmp(&float!(i)).unwrap(),
        _ => a.partial_cmp(b).unwrap(),
    });
    ret!(Value::Array(array))
}

fn get_type(data: Data, _: Args) -> Output {
    let t = unsafe { data.as_ref().get_type() };
    ret!(Value::String(t.to_owned()))
}
