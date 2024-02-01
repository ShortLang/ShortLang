use super::*;

use std::fs;
use std::io::Write;

pub fn init() {
    let mut set = INBUILT_METHODS.lock().unwrap();

    add_method![set,
        help: "Reads everything from the file.",
        "r" => [read, 0, Type::File],

        help: "Overwrites the contents of the file with the sepcified one.",
        "w" => [write, 1, Type::File],

        help: "Appends the contents to the file.",
        "a" => [append, 1, Type::File],
    ];
}

pub fn read(data: Data, _: Args) -> Output {
    let path = unsafe { data.as_ref() };

    let contents = match fs::read_to_string(path.to_string()) {
        Ok(c) => c,
        Err(e) => ret!(err: &format!("Failed to read '{}': '{}'", path.to_string(), e.kind())),
    };

    ret!(contents.into())
}

pub fn write(data: Data, args: Args) -> Output {
    let content = unsafe { args[0].as_ref() };
    let path = unsafe { data.as_ref() };

    match fs::write(path.to_string(), content.to_string()) {
        Err(e) => ret!(err: &format!("Failed to write to '{}': {}", path.to_string(), e.kind())),
        _ => {}
    }

    ret!()
}

pub fn append(data: Data, args: Args) -> Output {
    let content = unsafe { args[0].as_ref() };
    let path = unsafe { data.as_ref() };

    let mut file = match fs::OpenOptions::new()
        .write(true)
        .append(true)
        .open(path.to_string())
    {
        Ok(f) => f,
        Err(e) => ret!(err: &format!("Failed to open '{}': {}", path.to_string(), e.kind())),
    };

    match file.write_all(content.to_string().as_bytes()) {
        Err(e) => ret!(err: &format!("Failed to append to '{}': {}", path.to_string(), e.kind())),
        _ => {}
    }

    ret!();
}
