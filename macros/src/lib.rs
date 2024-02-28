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
    [ $set:expr, $(help: $help_msg:expr, $name:expr => [ $func:expr, $n:expr ] ),* $(,)? ] => {
        $(
            $set.insert((String::from($name), $n), (FnHandler::new($func), String::from($help_msg)));
        )*
    }
}

#[macro_export]
macro_rules! convert_to_i128 {
    [ $value:expr ] => {
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

#[macro_export]
macro_rules! cast {
    [ $val:expr, $ty:expr ] => {
        match $val.clone().try_cast($ty) { // TODO: ugh please remove this clone
            Some(val) => val,
            _ => ret!(err: &format!("Cannot cast '{}' type to '{}' type", $val.get_type(), $ty)),
        }
    };

    [ $val:expr => Array ] => {
        cast!($val, Type::Array).as_array()
    };

    [ $val:expr => String ] => {
        cast!($val, Type::String).as_str()
    };

    [ $val:expr => Int ] => {
        cast!($val, Type::Integer).as_int()
    };

    [ $val:expr => Float ] => {
        cast!($val, Type::Float).as_float()
    };

    [ $val:expr => Bool ] => {
        cast!($val, Type::Bool).as_bool()
    };
}

#[macro_export]
macro_rules! cast_nth_arg {
    [ $val:expr, $n:expr, $t:tt ] => {
        cast!(nth_arg!($val, $n) => $t)
    };
}

#[macro_export]
macro_rules! nth_arg {
    [ $args:expr, $n:expr ] => {
        unsafe { $args[$n].as_ref() }
    };
}

#[macro_export]
macro_rules! hook_fn {
    [ $set:expr, $fns:expr ] => {
        for (ptr, name, n, help) in $fns {
             $set.insert((name, n), (FnHandler::new(ptr), help));
        }
    };
}

#[macro_export]
macro_rules! hook_method {
    [ $set:expr, $methods:expr ] => {
        for (ptr, name, types, n, help) in $methods {
            let leaked = Box::leak(ptr); // Memory leaks are cool, sometimes

            if types == "*" {
                for i in Type::all_types() {
                    $set.insert(
                        (name.clone(), i.clone(), n),
                        (FieldFnHandler::new(leaked), help.clone())
                    );
                }

                continue;
            }

            for i in types
                .split(",")
                .map(|i| i.trim())
                .filter(|i| !i.is_empty())
                .map(|i| Type::try_from(i).expect("Invalid type"))
            {
                $set.insert(
                    (name.clone(), i, n),
                    (FieldFnHandler::new(leaked), help.clone())
                );
            }
        }
    };
}
