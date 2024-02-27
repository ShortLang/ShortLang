use az::SaturatingCast;
use rug::{ops::CompleteRound, Complete, Float, Integer};
use std::sync::Mutex;

use super::*;

lazy_static::lazy_static! {
    static ref RAND: Mutex<fastrand::Rng> = Mutex::new(fastrand::Rng::new());
}

#[shortlang_fn(args = 1, help = "Returns the length of array or string.")]
pub fn len(val: Input) -> Output {
    let len = cast!(nth_arg!(val, 0) => Array).len();
    ret!(Value::Int(len.into()))
}

#[shortlang_fn(name = "$$", args = 1, help = "Print to stdout without a newline character at the end.")]
pub fn print(val: Input) -> Output {
    print!("{}", nth_arg!(val, 0));
    std::io::Write::flush(&mut std::io::stdout()).expect("Failed to flush stdout");
    ret!()
}

#[shortlang_fn(name = "$", args = 1, help = "Print to stdout with a newline character at the end.")]
pub fn println(val: Input) -> Output {
    println!("{}", nth_arg!(val, 0));
    ret!()
}

#[shortlang_fn(args = 0, help = "Terminates the program at any given time. Returns with exit code 0")]
pub fn exit(_: Input) -> Output {
    std::process::exit(0);
}

#[shortlang_fn(name = "exit", args = 1, help = "Terminates the program at any given time. Returns with the specified exit code.")]
pub fn exit_code(val: Input) -> Output {
    std::process::exit(cast_nth_arg!(val, 0, Int).to_i32_wrapping());
}

#[shortlang_fn(name = "str", args = 1, help = "Tries to convert a value to string value, this function never fails.")]
pub fn to_str(val: Input) -> Output {
    ret!(Value::String(cast_nth_arg!(val, 0, String).to_owned()))
}

#[shortlang_fn(args = 1, help = "Returns the character associated with the ascii value.")]
pub fn ord(val: Input) -> Output {
    ret!(Value::Int(
        (cast_nth_arg!(val, 0, String).chars().next().unwrap() as u8).into()
    ))
}

#[shortlang_fn(args = 1, help = "Returns the ascii value associated with a character.")]
pub fn chr(val: Input) -> Output {
    ret!(Value::String(
        String::from_utf8(vec![cast_nth_arg!(val, 0, Int).to_u8().unwrap_or(b'\0')]).unwrap(),
    ))
}

/// Takes 1 parameter, 0..n or n..=0 if n < 0
#[shortlang_fn(args = 1, help = "Returns an array with values from 0 to x")]
pub fn rng(val: Input) -> Output {
    let upper_lim: i128 = convert_to_i128!(nth_arg!(val, 0));
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
#[shortlang_fn(name = "rng", args = 2, help = "Returns an array with values from a to b")]
pub fn rng_2(val: Input) -> Output {
    let [lower_lim, upper_lim] = [nth_arg!(val, 0), nth_arg!(val, 1)];
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
#[shortlang_fn(args = 0, help = "Returns a random number.")]
pub fn rnd(_val: Input) -> Output {
    ret!(Value::Int(RAND.lock().unwrap().i128(..).into()));
}

/// Takes 1 parameter
#[shortlang_fn(name = "rnd", args = 1, help = "Returns a random number between 0 and x")]
pub fn rnd_1(val: Input) -> Output {
    let upper_limit: i128 = unsafe { convert_to_i128!(nth_arg!(val, 0)) };
    ret!(Value::Int(RAND.lock().unwrap().i128(0..upper_limit).into()));
}

/// Takes 2 parameters
#[shortlang_fn(name = "rnd", args = 2, help = "Returns a random number between a and b")]
pub fn rnd_2(val: Input) -> Output {
    let [lower_lim, upper_lim]: [i128; 2] = unsafe {
        [
            convert_to_i128!(nth_arg!(val, 0)),
            convert_to_i128!(nth_arg!(val, 1)),
        ]
    };

    ret!(Value::Int(
        RAND.lock().unwrap().i128(lower_lim..upper_lim).into()
    ));
}

#[shortlang_fn(name = "flt", args = 1, help = "Tries to convert a value to floating point value, returns an error if conversion is not possible.")]
pub fn to_float(val: Input) -> Output {
    let val = nth_arg!(val, 0);
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

#[shortlang_fn(name = "int", args = 1, help = "Tries to convert a value to integer value, returns an error if conversion is not possible.")]
pub fn to_int(val: Input) -> Output {
    let val = nth_arg!(val, 0);
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

#[shortlang_fn(name = "inp", args = 0, help = "Read a line from stdin without any prompt.")]
pub fn input(val: Input) -> Output {
    use std::io::*;

    let mut s = String::new();
    match stdin().read_line(&mut s) {
        Err(x) => ret!(err: &x.to_string()),
        Ok(_) => {}
    };

    ret!(Value::String(s.trim().to_string()));
}

#[shortlang_fn(name = "inp", args = 1, help = "Read a line from stdin, with a prompt.")]
pub fn input_with_prompt(val: Input) -> Output {
    use std::io::*;
    let prompt = cast_nth_arg!(val, 0, String);

    print!("{}", prompt);
    stdout().flush().unwrap();

    let mut s = String::new();
    match stdin().read_line(&mut s) {
        Err(x) => ret!(err: &x.to_string()),
        Ok(_) => {}
    };

    ret!(Value::String(s.trim().to_string()));
}

#[shortlang_fn(name = "type", args = 1, help = "Returns the type of the value.")]
pub fn get_type(val: Input) -> Output {
    let t = nth_arg!(val, 0).get_type();
    ret!(Value::String(t.to_owned()));
}

#[shortlang_fn(args = 1, help = "Opens a file and returns a file object.")]
pub fn open(val: Input) -> Output {
    let path = cast_nth_arg!(val, 0, String);
    ret!(Value::File(path))
}
