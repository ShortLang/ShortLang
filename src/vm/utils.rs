use std::ptr::NonNull;
use std::{collections::HashMap, ops::Range};

use super::{memory::alloc_new_value, value::Value, vm::VarId};

#[macro_export]
macro_rules! for_each_arg {
    { $arg:ident, 0, Some($e:ident) => { $($some:tt)* }, None => { $($none:tt)* } } => {
        println!("doin' nothin'");
    };

    { $arg:ident, $n:expr, Some($e:ident) => { $($some:tt)* }, None => { $($none:tt)* } } => {
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

    { $arg:ident, $e:ident => { $($b:tt)* }} => {
        $arg.as_ref()
            .unwrap_or(&vec![])
            .into_iter()
            .cloned()
            .for_each(|$e| $($b)*)
    };
}

#[macro_export]
macro_rules! float {
    ($val:expr) => {
        rug::Float::with_val(53, $val)
    };
}

#[macro_export]
macro_rules! process_placeholder {
    { $self:ident, $placeholder:expr, $span:expr } => {
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

#[derive(Debug, Clone)]
pub(crate) struct FunctionData {
    pub name: String,
    pub parameters: Vec<(String, VarId)>,
    pub instruction_range: Range<usize>,
    pub scope_idx: usize,
    pub returns: bool,
}

impl FunctionData {
    pub fn get_var_names(&self) -> Vec<&str> {
        self.parameters
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
    }

    pub fn get_var_ids(&self) -> Vec<VarId> {
        self.parameters
            .iter()
            .map(|(_, id)| *id)
            .collect::<Vec<_>>()
    }
}

pub fn allocate(val: Value) -> NonNull<Value> {
    NonNull::new(alloc_new_value(val)).expect("Failed to allocate")
}

#[derive(Debug, Clone)]
pub(crate) struct FnStackData {
    pub(crate) pc_before: usize,
    pub(crate) scope_idx: usize,
    pub(crate) previous_stack_len: usize,
    pub(crate) variables_id: HashMap<String, u32>,
    pub(crate) variables: HashMap<u32, Option<NonNull<Value>>>,
}
