use az::SaturatingCast;
use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::utils::*;
use super::*;

pub fn len(val: Input) -> Output {
    let len = unsafe { val[0].as_ref().as_array().len() };

    ret!(Value::Int(len.into()))
}

pub fn print(val: Input) -> Output {
    print!("{}", unsafe { val[0].as_ref() });
    std::io::Write::flush(&mut std::io::stdout()).expect("Failed to flush stdout");

    ret!()
}

pub fn println(val: Input) -> Output {
    println!("{}", unsafe { val[0].as_ref() });

    ret!()
}

pub fn exit(_: Input) -> Output {
    std::process::exit(0);
}

pub fn to_str(val: Input) -> Output {
    ret!(Value::String(unsafe { val[0].as_ref().to_string() }))
}

pub fn ord(val: Input) -> Output {
    ret!(Value::Int(
        unsafe { val[0].as_ref().as_str().chars().next().unwrap() as u8 }.into(),
    ))
}

pub fn char(val: Input) -> Output {
    ret!(Value::String(
        String::from_utf8(vec![unsafe {
            val[0].as_ref().as_int().to_u8().unwrap_or(b'\0')
        }])
        .unwrap(),
    ))
}

pub fn rng(val: Input) -> Output {
    let &[start, end] = val else { unreachable!() };
    let [start, end] = unsafe { [start.as_ref(), end.as_ref()] };

    match (start, end) {
        (Value::Int(start), Value::Int(end)) => {
            let start_value: i64 = start.saturating_cast();
            let end_value: i64 = end.saturating_cast();
            let mut array = vec![];
            if start_value > end_value {
                for i in (end_value + 1..=start_value).rev() {
                    array.push(Value::Int(Integer::from(i)));
                }
            } else {
                for i in start_value..end_value {
                    array.push(Value::Int(Integer::from(i)));
                }
            }

            ret!(Value::Array(array));
        }
        (Value::Nil, Value::Int(end)) => {
            let end_value: i64 = end.saturating_cast();
            let mut array = vec![];
            if 0 > end_value {
                for i in (end_value + 1..=0).rev() {
                    array.push(Value::Int(Integer::from(i)));
                }
            } else {
                for i in 0..end_value {
                    array.push(Value::Int(Integer::from(i)));
                }
            }

            ret!(Value::Array(array));
        }

        _ => ret!(err: "Expected an integer"),
    }
}

pub fn rnd(val: Input) -> Output {
    let [popped2, popped1] = val else {
        unreachable!()
    };

    let [popped2, popped1] = unsafe { [popped2.as_ref(), popped1.as_ref()] };

    let Ok(mut end) = convert_to_i128(popped1) else {
        ret!(err: "Expected a number".to_owned())
    };

    let Ok(mut start) = convert_to_i128(popped2) else {
        ret!(err: "Expected a number".to_owned())
    };

    if start == end {
        ret!(Value::Int(Integer::from(start)));
    } else {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }

        ret!(Value::Int(Integer::from(
            fastrand::Rng::new().i128(start..end)
        )));
    }
}

pub fn to_float(val: Input) -> Output {
    let val = unsafe { val[0].as_ref() };

    ret!(Value::Float(match val {
        Value::Int(i) => float!(i),
        Value::Float(f) => f.clone(),
        Value::Bool(b) => float!(*b as i32),
        Value::String(s) => match Float::parse(s) {
            Ok(i) => i.complete(53),
            Err(e) => ret!(err:
                &format!("cannot parse the string to float value, {}", e)
            ),
        },

        Value::Nil => Float::new(53),
        Value::Array(_) => ret!(err: "Cannot convert array type to float"),
        Value::File(_) => ret!(err: "Cannot convert file type to float"),
    }));
}

pub fn to_int(val: Input) -> Output {
    let val = unsafe { val[0].as_ref() };
    ret!(Value::Int(match val {
        Value::Int(i) => i.clone(),
        Value::Float(f) => f.to_integer().unwrap(),
        Value::Bool(b) => Integer::from(*b as i32),
        Value::String(s) => match Integer::parse(s) {
            Ok(i) => i.complete(),
            Err(e) => ret!(err:
                &format!("cannot parse the string to int value, {}", e)
            ),
        },

        Value::Nil => Integer::new(),
        Value::Array(_) => ret!(err:"Cannot convert array type to int"),
        Value::File(_) => ret!(err:"Cannot convert file type to int"),
    }));
}

pub fn input(val: Input) -> Output {
    use std::io::*;

    let prompt = unsafe { val[0].as_ref() };

    match prompt {
        Value::Nil => {}
        _ => {
            print!("{}", prompt.to_string());
            stdout().flush().unwrap();
        }
    }

    let mut s = String::new();
    match stdin().read_line(&mut s) {
        Err(x) => ret!(err: &x.to_string()),
        Ok(_) => {}
    };

    ret!(Value::String(s.trim().to_string()));
}

pub fn get_type(val: Input) -> Output {
    let t = unsafe { val[0].as_ref().get_type() };
    ret!(Value::String(t.to_owned()));
}

pub fn open(val: Input) -> Output {
    let path = unsafe { val[0].as_ref().to_string() };
    ret!(Value::File(path))
}
