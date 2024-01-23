#[macro_export]
macro_rules! inbuilt_methods {
    [ $self:ident, $names:expr, $args:ident, $([ $fn_name:expr => [$($ty:expr),+ $(,)?], $num_args:expr, $span:expr, { $($preprocess:tt)* } ]),*, _ => { $($tt:tt)* } $(,)? ] => {
        match $names {
            $(
                $fn_name => {
                    { $($preprocess)* }

                    for_each_arg!($args, $num_args,
                        Some(e) => { $self.compile_expr(e) },
                        None => { $self.stack.push(allocate(Value::Nil)) }
                    );

                    $self.instructions.push((Instr(Bytecode::Method(MethodFunction {
                        name: $fn_name.to_string(),
                        on_types: vec![$($ty,)*],
                        num_args: $num_args,
                        in_built: true,
                    }), vec![]), $span));
                }
            )*

            _ => { $($tt)* }
        }
    }
}

#[macro_export]
macro_rules! inbuilt_fn {
    [ $self:ident, $names:expr, $args:ident, $span:expr, $([ $fn_name:expr, $num_args:expr ]),*, _ => { $($tt:tt)* } $(,)? ] => {
        match $names.as_str() {
            $(
                $fn_name => {
                    INBUILT_FUNCTIONS.lock().unwrap().insert(String::from($fn_name));

                    for_each_arg!($args, $num_args,
                        Some(e) => { $self.compile_expr(e) },
                        None => { $self.stack.push(allocate(Value::Nil)) }
                    );

                    $self.instructions.push((Instr(Bytecode::BuiltInFunction(String::from($fn_name)), vec![]), $span));
                }
            )*

            _ => { $($tt)* }
        }
    }
}

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
