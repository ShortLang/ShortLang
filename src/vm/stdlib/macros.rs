#[macro_export]
macro_rules! ret {
    [] => {
        return Ok(None)
    };

    [ $val:expr ] => {
        return Ok(Some(allocate($val)))
    };

    [ err: $msg:expr ] => {
        return Err(String::from($msg))
    };
}
