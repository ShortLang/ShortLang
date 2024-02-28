#![allow(unused)]

use lazy_static;
use proc_macro::TokenStream;
use quote::quote;
use std::str::FromStr;
use std::sync::Mutex;
use syn;

use value_ty::*;

use darling::ast::NestedMeta;
use darling::{Error, FromMeta};

mod structs;

use structs::*;

lazy_static::lazy_static! {
    static ref FUNCTION_NAMES: Mutex<Vec<FnData>> = Mutex::new(Vec::new());
    static ref METHOD_NAMES: Mutex<Vec<MethodFnData>> = Mutex::new(Vec::new());
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

    FUNCTION_NAMES.lock().unwrap().push(args);

    quote!(#input).into()
}

#[proc_macro_attribute]
pub fn shortlang_method(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };

    let mut args = match MethodFnData::from_list(&attr_args) {
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

    METHOD_NAMES.lock().unwrap().push(args);

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
    } in FUNCTION_NAMES.lock().unwrap().iter()
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

#[proc_macro]
pub fn get_methods(args: TokenStream) -> TokenStream {
    let mut fn_ptrs = vec![];

    for MethodFnData {
        help,
        name,
        ident_name,
        args,
        types,
    } in METHOD_NAMES.lock().unwrap().iter()
    {
        let ident_name = quote::format_ident!("{ident_name}");

        // #(#path_segments)::*
        let fn_ptr = quote! {
            (
                Box::new(#ident_name) as Box<dyn Fn(std::ptr::NonNull<Value>, &[std::ptr::NonNull<Value>]) -> Result<Option<std::ptr::NonNull<Value>>, String>>,
                #name.to_string(),
                #types.to_string(),
                #args,
                #help.to_string(),
            )
        };

        fn_ptrs.push(fn_ptr);
    }

    let expanded = quote! {
        {
            fn get_method_pointers() ->
            Vec<(
                Box<dyn Fn(std::ptr::NonNull<Value>, &[std::ptr::NonNull<Value>]) -> Result<Option<std::ptr::NonNull<Value>>, String>>,
                String,
                String,
                usize,
                String
            )> {
                vec![#(#fn_ptrs),*]
            }

            get_method_pointers()
        }
    };

    expanded.into()
}
