#[macro_export]
macro_rules! for_each_arg {
    [ $arg:ident, $n:expr, Some($e:ident) => { $($some:tt)* }, None => { $($none:tt)* } ] => {
        $arg.as_ref()
            .unwrap_or(&vec![])
            .into_iter()
            .cloned()
            .map(|i| Some(i))
            .chain(std::iter::repeat(None))
            .take($n)
            .for_each(|i| match i {
                Some($e) => $($some)*,
                None => $($none)*,
            }
        )
    };

    [ $arg:ident, $e:ident => { $($b:tt)* } ] => {
        $arg.as_ref()
            .unwrap_or(&vec![])
            .into_iter()
            .cloned()
            .for_each(|$e| $($b)*)
    };
}

#[macro_export]
macro_rules! float {
    [ $val:expr ] => {
        rug::Float::with_val(53, $val)
    };
}

#[macro_export]
macro_rules! process_placeholder {
    [ $self:ident, $placeholder:expr, $span:expr ] => {
        let parsed_exprs = PParser::new(
            $placeholder,
            LogosToken::lexer($placeholder)
                .spanned()
                .map(|(tok, span)| match tok {
                    Ok(tok) => (tok, span.into()),
                    Err(()) => (LogosToken::Error, span.into()),
                })
                .collect::<Vec<_>>(),
        )
        .parse()
        .into_iter()
        .map(|mut i| {
            i.span = $span.clone();
            i
        })
        .collect::<Vec<_>>();

        for expr in parsed_exprs {
            $self.compile_expr(expr);
        }
    };
}

#[macro_export]
macro_rules! to_usize {
    [ $index:expr, $length:expr ] => {
        if $index < 0 {
            (($index % $length + $length) % $length).to_usize_wrapping()
        } else {
            $index.to_usize_wrapping()
        }
    };
}

#[macro_export]
macro_rules! add_method {
    [ $set:expr, $(help: $help_msg:expr, $name:expr => [ $func:expr, $n:expr, $( $type:expr ),* $(,)? ] ),* $(,)? ] => {
        $(
            $(
                $set.insert(
                    (String::from($name), $type, $n),
                    (
                        FieldFnHandler::new($func),
                        String::from($help_msg)
                    )
                );
            )*
        )*
    };
}

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
    // [ $set:expr, $( $name:expr => [$placeholder:ident; $n:expr] { $($tt:tt)* } ),* $(,)? ] => {
    //     $(
    //         $set.insert((String::from($name), $n), FnHandler::new(
    //             |$placeholder| unsafe {
    //                 $($tt)*
    //             }
    //         ));
    //     )*
    // };

    [ $set:expr, $(help: $help_msg:expr, $name:expr => [ $func:expr, $n:expr ] ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), (FnHandler::new($func), String::from($help_msg)));
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
