#![allow(unused)]

use lazy_static;
use proc_macro::TokenStream;
use quote::quote;
use std::str::FromStr;
use std::sync::Mutex;
use syn;

use value_ty::Value;

use darling::ast::NestedMeta;
use darling::{Error, FromMeta};

lazy_static::lazy_static! {
    static ref NAMES: Mutex<Vec<FnData>> = Mutex::new(Vec::new());
}

type FnPtr =
    Box<dyn Fn(&[std::ptr::NonNull<Value>]) -> Result<Option<std::ptr::NonNull<Value>>, String>>;

#[derive(Debug, FromMeta)]
struct FnData {
    #[darling(default)]
    help: String,

    #[darling(default)]
    name: String,
    args: usize,

    #[darling(default, skip)]
    ident_name: String,
}

#[proc_macro_attribute]
pub fn shortlang_fn(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };

    let mut args = match FnData::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let input: syn::ItemFn = syn::parse(input).unwrap();
    args.ident_name = input.sig.ident.to_string();

    if args.name.is_empty() {
        args.name = args.ident_name.clone();
    }

    NAMES.lock().unwrap().push(args);

    quote!(#input).into()
}

#[proc_macro]
pub fn get_functions(args: TokenStream) -> TokenStream {
    let mut fn_ptrs = vec![];

    for FnData {
        help,
        name,
        ident_name,
        args,
    } in NAMES.lock().unwrap().iter()
    {
        let ident_name = quote::format_ident!("{ident_name}");

        // #(#path_segments)::*
        let fn_ptr = quote! {
            (
                Box::new(#ident_name) as Box<dyn Fn(&[std::ptr::NonNull<Value>]) -> Result<Option<std::ptr::NonNull<Value>>, String>>,
                #name.to_string(),
                #args,
                #help.to_string(),
            )
        };

        fn_ptrs.push(fn_ptr);
    }

    let expanded = quote! {
        {
            fn get_function_pointers() ->
            Vec<(
                Box<dyn Fn(&[std::ptr::NonNull<Value>]) -> Result<Option<std::ptr::NonNull<Value>>, String>>,
                String,
                usize,
                String
            )> {
                vec![#(#fn_ptrs),*]
            }

            get_function_pointers()
        }
    };

    expanded.into()
}
