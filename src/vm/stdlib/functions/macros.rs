#[macro_export]
macro_rules! ret {
    [] => {
        return Ok(None)
    };

    [ ptr: $ptr:expr ] => {
        return Ok(Some(unsafe {
            NonNull::new_unchecked($ptr as *mut _)
        }))
    };

    [ $val:expr ] => {
        return Ok(Some(allocate($val)))
    };

    [ err: $msg:expr ] => {
        return Err(String::from($msg))
    };
}

#[macro_export]
macro_rules! add_fn {
    [ $set:expr, $( $name:expr => [$placeholder:ident; $n:expr] { $($tt:tt)* } ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), FnHandler::new(
                |$placeholder| unsafe {
                    $($tt)*
                }
            ));
        )*
    };

    [ $set:expr, $( $name:expr => [ $func:expr, $n:expr ] ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), FnHandler::new($func));
        )*
    }
}

#[macro_export]
macro_rules! convert_to_i128 {
    ($value:expr) => {
        match $value {
            Value::Int(i) => i.saturating_cast(),
            Value::Float(f) => f
                .to_integer()
                .unwrap_or_else(|| {
                    // Only way for the unwrap to fail is if the float is infinity
                    if f.is_sign_negative() {
                        Integer::from(i128::MIN)
                    } else {
                        Integer::from(i128::MAX)
                    }
                })
                .saturating_cast(),
            _ => ret!(err: "Expected a number"),
        }
    };
}
