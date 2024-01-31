use super::*;

pub fn init() {
    let mut set = INBUILT_METHODS.lock().unwrap();

    add_method![set,
        "sin" => [sine, 0, Type::Integer, Type::Float],
        "cos" => [cosine, 0, Type::Integer, Type::Float],
        "tan" => [tangent, 0, Type::Integer, Type::Float],
        "cot" => [cotangent, 0, Type::Integer, Type::Float],
        "sec" => [secant, 0, Type::Integer, Type::Float],
        "csc" => [cosecant, 0, Type::Integer, Type::Float],
    ];
}

fn sine(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.sin().into())
}

fn cosine(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.cos().into())
}

fn tangent(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.tan().into())
}

fn cotangent(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.cot().into())
}

fn secant(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.sec().into())
}

fn cosecant(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().as_float() };
    ret!(number.csc().into())
}
