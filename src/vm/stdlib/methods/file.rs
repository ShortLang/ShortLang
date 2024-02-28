use super::*;

use std::fs;
use std::io::Write;

#[shortlang_method(
    name = "r",
    args = 0,
    help = "Reads everything from the file.",
    types = "file"
)]
pub fn read(data: Data, _: Args) -> Output {
    let path = unsafe { data.as_ref() };

    let contents = match fs::read_to_string(path.to_string()) {
        Ok(c) => c,
        Err(e) => ret!(err: &format!("Failed to read '{}': '{}'", path.to_string(), e.kind())),
    };

    ret!(contents.into())
}

#[shortlang_method(
    name = "w",
    args = 1,
    help = "Writes the contents to the file.",
    types = "file"
)]
pub fn write(data: Data, args: Args) -> Output {
    let content = nth_arg!(args, 0);
    let path = unsafe { data.as_ref() };

    match fs::write(path.to_string(), content.to_string()) {
        Err(e) => ret!(err: &format!("Failed to write to '{}': {}", path.to_string(), e.kind())),
        _ => {}
    }

    ret!()
}

#[shortlang_method(
    name = "a",
    args = 1,
    help = "Appends the contents to the file.",
    types = "file"
)]
pub fn append(data: Data, args: Args) -> Output {
    let content = nth_arg!(args, 0);
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
