use super::*;

pub fn init() {
    let mut functions = INBUILT_METHODS.lock().unwrap();

    add_method![functions,
        "push" => [push, 1, Type::String, Type::Array],
        "join" => [join, 1, Type::Array],
        "split" => [split, 1, Type::String],

        "pop" => [pop, 0, Type::String, Type::Array],
        "clear" => [clear, 0, Type::String, Type::Array],
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
    let Some(val) = (match unsafe { data.as_mut() } {
        Value::Array(a) => a.pop(),
        Value::String(s) => s.pop().map(|i| Value::String(i.to_string())),
        _ => unreachable!(),
    }) else {
        ret!(err: "Cannot pop empty array");
    };

    ret!(val);
}

fn clear(mut data: Data, _: Args) -> Output {
    if !unsafe { data.as_mut().clear() } {
        ret!(err: "Attempt to pop an empty value")
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

fn get_type(data: Data, _: Args) -> Output {
    let t = unsafe { data.as_ref().get_type() };
    ret!(Value::String(t.to_owned()))
}
