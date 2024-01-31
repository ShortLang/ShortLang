use super::*;

pub fn init() {
    let mut set = INBUILT_METHODS.lock().unwrap();

    add_method![set,
        help: "Computes the sine",
        "sin" => [sine, 0, Type::Integer, Type::Float],

        help: "Computes the cosine",
        "cos" => [cosine, 0, Type::Integer, Type::Float],

        help: "Computes the tangent",
        "tan" => [tangent, 0, Type::Integer, Type::Float],

        help: "Computes the cotangent",
        "cot" => [cotangent, 0, Type::Integer, Type::Float],

        help: "Computes the secant",
        "sec" => [secant, 0, Type::Integer, Type::Float],

        help: "Computes the cosecant",
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
