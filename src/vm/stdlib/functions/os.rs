use super::*;

extern "C" {
    fn system(_: *mut u8) -> i32;
}

pub fn init() {
    let mut ib = INBUILT_FUNCTIONS.lock().unwrap();

    add_fn![ib,
        help: "Executes a shell command.",
        "run" => [run, 1],

        help: "Returns the command-line arguments given to the program.",
        "arg" => [args, 0],

        help: "Gets all the environment variables as a key-value pair.",
        "env" => [list_vars, 0],

        help: "Gets the value of the specified environment variable",
        "env" => [get_env, 1],

        help: "Sets the value of the specified environment variable.",
        "env" => [set_env, 2],
    ];
}

fn run(val: Input) -> Output {
    let input = unsafe { val[0].as_ref().as_str() };
    let status = unsafe { system(input.as_ptr() as *mut _) };

    ret!(Value::Int(status.into()))
}

fn args(_: Input) -> Output {
    ret!(Value::Array(
        std::env::args().map(|i| i.into()).collect::<Vec<Value>>()
    ))
}

fn get_env(val: Input) -> Output {
    let var_name = unsafe { val[0].as_ref().as_str() };
    let env = std::env::var(var_name).unwrap_or("".to_owned());
    if env.is_empty() {
        ret!(Value::Nil)
    }
    ret!(Value::String(env))
}

fn set_env(val: Input) -> Output {
    let [key, value] = unsafe { [val[0].as_ref().as_str(), val[1].as_ref().as_str()] };
    let old_value = std::env::var(key).unwrap_or("".to_owned());
    std::env::set_var(key, value);
    if old_value.is_empty() {
        ret!(Value::Nil)
    }
    ret!(Value::String(old_value))
}

fn list_vars(_: Input) -> Output {
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
