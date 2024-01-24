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

#[macro_export]
macro_rules! add_fn {
    [ $set:expr, $( $name:expr => [$placeholder:ident; $n:expr] { $($tt:tt)* } ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), Handler::new(
                |$placeholder| unsafe {
                    $($tt)*
                }
            ));
        )*
    };

    [ $set:expr, $( $name:expr => [ $func:expr, $n:expr ] ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), Handler::new($func));
        )*
    }
}
