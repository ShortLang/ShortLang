use super::*;

extern "C" {
    fn system(_: *mut u8) -> i32;
}

#[shortlang_fn(args = 1, help = "Executes a shell command.")]
pub fn run(val: Input) -> Output {
    let input = cast_nth_arg!(val, 0, String);
    let status = unsafe { system(input.as_ptr() as *mut _) };

    ret!(Value::Int(status.into()))
}

#[shortlang_fn(
    args = 0,
    help = "Returns the command-line arguments given to the program."
)]
pub fn args(_: Input) -> Output {
    ret!(Value::Array(
        std::env::args().map(|i| i.into()).collect::<Vec<Value>>()
    ))
}

#[shortlang_fn(
    name = "env",
    args = 1,
    help = "Gets the value of the specified environment variable."
)]
pub fn get_env(val: Input) -> Output {
    let var_name = cast_nth_arg!(val, 0, String);
    let env = std::env::var(var_name).unwrap_or("".to_owned());
    if env.is_empty() {
        ret!(Value::Nil)
    }

    ret!(Value::String(env))
}

#[shortlang_fn(
    name = "env",
    args = 2,
    help = "Sets the value of the specified environment variable."
)]
pub fn set_env(val: Input) -> Output {
    let [key, value] = [cast_nth_arg!(val, 0, String), cast_nth_arg!(val, 1, String)];
    let old_value = std::env::var(&key).unwrap_or("".to_owned());
    std::env::set_var(&key, value);

    if old_value.is_empty() {
        ret!(Value::Nil)
    }

    ret!(Value::String(old_value))
}

#[shortlang_fn(
    name = "env",
    args = 0,
    help = "Gets all the environment variables as an array of key-value pairs."
)]
pub fn list_vars(_: Input) -> Output {
    let vars = std::env::vars_os()
        .into_iter()
        .map(|(key, value)| {
            Value::Array(vec![
                key.into_string().unwrap().into(),
                value.into_string().unwrap().into(),
            ])
        })
        .collect::<Vec<_>>();

    ret!(Value::Array(vars))
}
