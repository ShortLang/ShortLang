#[macro_export]
macro_rules! add_method {
    [ $set:expr, $( $name:expr => [ $func:expr, $n:expr, $( $type:expr ),* $(,)? ] ),* $(,)? ] => {
        $(
            $(
                $set.insert(
                    (String::from($name), $type, $n),
                    MethodFnHandler::new($func)
                );
            )*
        )*
    };
}
