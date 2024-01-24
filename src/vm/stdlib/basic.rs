use az::SaturatingCast;
use rug::{ops::CompleteRound, Complete, Float, Integer};
use std::sync::Mutex;

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
    let upper_lim: i128 = convert_to_i128!(unsafe { val[0].as_ref() });
    let mut array = Vec::new();
    if 0 > upper_lim {
        for i in (upper_lim..0).rev() {
            array.push(Value::Int(i.into()));
        }
    } else {
        for i in 0..upper_lim {
            array.push(Value::Int(i.into()));
        }
    }
    ret!(Value::Array(array));
}

/// Takes 2 parameters, 0..n or n..=0 if n < 0
pub fn rng_2(val: Input) -> Output {
    let [lower_lim, upper_lim] = unsafe { [val[0].as_ref(), val[1].as_ref()] };
    let lower_lim: i128 = convert_to_i128!(lower_lim);
    let upper_lim: i128 = convert_to_i128!(upper_lim);
    let mut array = Vec::new();
    if lower_lim > upper_lim {
        for i in (upper_lim..lower_lim).rev() {
            array.push(Value::Int(i.into()));
        }
    } else {
        for i in lower_lim..upper_lim {
            array.push(Value::Int(i.into()));
        }
    }
    ret!(Value::Array(array));
}

/// Takes 0 parameters
pub fn rnd_0(_val: Input) -> Output {
    ret!(Value::Int(RAND.lock().unwrap().i128(..).into()));
}

/// Takes 1 parameter
pub fn rnd_1(val: Input) -> Output {
    let upper_limit: i128 = unsafe { convert_to_i128!(val[0].as_ref()) };
    ret!(Value::Int(RAND.lock().unwrap().i128(0..upper_limit).into()));
}

/// Takes 2 parameters
pub fn rnd_2(val: Input) -> Output {
    let [lower_lim, upper_lim]: [i128; 2] = unsafe {
        [
            convert_to_i128!(val[0].as_ref()),
            convert_to_i128!(val[1].as_ref()),
        ]
    };
    ret!(Value::Int(
        RAND.lock().unwrap().i128(lower_lim..upper_lim).into()
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
