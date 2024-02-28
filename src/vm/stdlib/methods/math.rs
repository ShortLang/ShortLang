use super::*;

#[shortlang_method(
    name = "sin",
    args = 0,
    types = "int, float",
    help = "Computes the sine."
)]
pub fn sine(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(dbg!(number.sin()).into())
}

#[shortlang_method(
    name = "cos",
    args = 0,
    types = "int, float",
    help = "Computes the cosine."
)]
pub fn cosine(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(number.cos().into())
}

#[shortlang_method(
    name = "tan",
    args = 0,
    types = "int, float",
    help = "Computes the tangent."
)]
pub fn tangent(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(number.tan().into())
}

#[shortlang_method(
    name = "cot",
    args = 0,
    types = "int, float",
    help = "Computes the cotangent."
)]
pub fn cotangent(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(number.cot().into())
}

#[shortlang_method(
    name = "sec",
    args = 0,
    types = "int, float",
    help = "Computes the secant."
)]
pub fn secant(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(number.sec().into())
}

#[shortlang_method(
    name = "csc",
    args = 0,
    types = "int, float",
    help = "Computes the cosecant."
)]
pub fn cosecant(number: Data, _: Args) -> Output {
    let number = unsafe { number.as_ref().clone().as_float() };
    ret!(number.csc().into())
}
