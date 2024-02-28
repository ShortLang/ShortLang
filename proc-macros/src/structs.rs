use darling::ast::NestedMeta;
use darling::{Error, FromMeta};

#[derive(Debug, FromMeta)]
pub struct FnData {
    #[darling(default)]
    pub help: String,

    #[darling(default)]
    pub name: String,
    pub args: usize,

    #[darling(default, skip)]
    pub ident_name: String,
}

#[derive(Debug, FromMeta)]
pub struct MethodFnData {
    #[darling(default)]
    pub help: String,

    #[darling(default)]
    pub name: String,

    pub args: usize,

    #[darling(default, skip)]
    pub ident_name: String,

    #[darling(rename = "types")]
    pub types: String,
}
