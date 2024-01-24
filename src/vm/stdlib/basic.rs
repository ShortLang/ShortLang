use std::sync::Mutex;

use rug::{ops::CompleteRound, Complete, Float, Integer};

use super::*;

lazy_static::lazy_static! {
    static ref RAND: Mutex<fastrand::Rng> = Mutex::new(fastrand::Rng::new());
}

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

/// Takes 1 parameter, 0..n or n..=0 if n < 0
pub fn rng_1(val: Input) -> Output {
    let upper_lim = unsafe { val[0].as_ref().as_int().to_i128_wrapping() };

    let reverse = upper_lim.is_negative();

    let mut array = (0..upper_lim.abs())
        .map(|i| Value::Int((if reverse { -i } else { i }).into()))
        .collect::<Vec<_>>();

    if reverse {
        array.reverse();
    }

    ret!(Value::Array(array));
}

/// Takes 2 parameters, 0..n or n..=0 if n < 0
pub fn rng_2(val: Input) -> Output {
    // TODO: add negative number support

    let [lower_lim, upper_lim] = unsafe {
        [
            val[0].as_ref().as_int().to_i128_wrapping(),
            val[1].as_ref().as_int().to_i128_wrapping(),
        ]
    };

    let reverse = lower_lim > upper_lim;

    let low = std::cmp::min(lower_lim, upper_lim);
    let high = std::cmp::max(lower_lim, upper_lim);

    let mut array = (low.abs()..high.abs())
        .map(|i| Value::Int(i.into()))
        .collect::<Vec<_>>();

    if reverse {
        array.reverse();
    }

    ret!(Value::Array(array));
}

/// Takes 0 parameters
pub fn rnd_0(_val: Input) -> Output {
    ret!(Value::Int(RAND.lock().unwrap().i128(..).into()));
}

/// Takes 1 parameter
pub fn rnd_1(val: Input) -> Output {
    let upper_limit = unsafe { val[0].as_ref().as_int() };
    ret!(Value::Int(
        RAND.lock()
            .unwrap()
            .i128(0..upper_limit.to_i128_wrapping())
            .into()
    ));
}

/// Takes 2 parameters
pub fn rnd_2(val: Input) -> Output {
    let [lower_lim, upper_lim] = unsafe { [val[0].as_ref().as_int(), val[1].as_ref().as_int()] };

    ret!(Value::Int(
        RAND.lock()
            .unwrap()
            .i128(lower_lim.to_i128_wrapping()..upper_lim.to_i128_wrapping())
            .into()
    ));
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
