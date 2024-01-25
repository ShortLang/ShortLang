use super::*;

pub fn get_env(val: Input) -> Output {
    let var_name = unsafe { val[0].as_ref().as_str() };
    let env = std::env::var(var_name).unwrap_or("".to_owned());

    ret!(Value::String(env))
}

pub fn set_env(val: Input) -> Output {
    let [key, value] = unsafe { [val[0].as_ref().as_str(), val[1].as_ref().as_str()] };

    let old_value = std::env::var(key).unwrap_or("".to_owned());

    std::env::set_var(key, value);

    ret!(Value::String(old_value))
}

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
