use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    AngleBracketedGenericArguments, DeriveInput, Expr, ExprLit, GenericArgument, Ident, ImplItem,
    ItemImpl, Lit, MetaNameValue, PathArguments, Result, Token, Type, TypePath,
};

/// Struct for holding the parsed attribute arguments.
struct Attributes {
    base_offset: usize,
    report_id: u8,
    cmd_len: usize,
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse the comma-separated list of key = value pairs.
        let args = Punctuated::<MetaNameValue, Token![,]>::parse_terminated(input)?;
        let mut base_offset_opt = None;
        let mut report_id_opt = None;
        let mut cmd_len_opt = None;

        for arg in args {
            // Get the key as a string.
            let key = arg
                .path
                .get_ident()
                .ok_or_else(|| syn::Error::new_spanned(&arg.path, "Expected identifier"))?
                .to_string();

            // For each arg, we now extract the literal from the expression.
            let lit_int = if let Expr::Lit(ExprLit {
                lit: Lit::Int(ref i),
                ..
            }) = arg.value
            {
                i
            } else {
                return Err(syn::Error::new_spanned(
                    &arg.value,
                    "Expected integer literal",
                ));
            };

            match key.as_str() {
                "base_offset" => {
                    base_offset_opt = Some(lit_int.base10_parse()?);
                }
                "report_id" => {
                    report_id_opt = Some(lit_int.base10_parse()?);
                }
                "cmd_len" => {
                    cmd_len_opt = Some(lit_int.base10_parse()?);
                }
                _ => return Err(syn::Error::new_spanned(arg, "Unknown attribute key")),
            }
        }

        // Ensure all required fields were provided.
        let base_offset = base_offset_opt
            .ok_or_else(|| syn::Error::new(input.span(), "Missing `base_offset`"))?;
        let report_id =
            report_id_opt.ok_or_else(|| syn::Error::new(input.span(), "Missing `report_id`"))?;
        let cmd_len =
            cmd_len_opt.ok_or_else(|| syn::Error::new(input.span(), "Missing `cmd_len`"))?;

        Ok(Attributes {
            base_offset,
            report_id,
            cmd_len,
        })
    }
}

#[proc_macro_derive(CommandDescriptor, attributes(command_descriptor))]
pub fn derive_my_trait(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let mut args_opt = None;
    for attr in ast.attrs.iter() {
        if attr.path().is_ident("command_descriptor") {
            // Instead of parse_meta, we use parse_args to parse the tokens within parentheses.
            let args: Attributes = attr
                .parse_args()
                .expect("Failed to parse command_descriptor arguments");
            args_opt = Some(args);
            break;
        }
    }

    let args = args_opt.expect("Missing #[command_descriptor(...)] attribute");
    let base_offset = args.base_offset;
    let report_id = args.report_id;
    let cmd_len = args.cmd_len;

    let name = &ast.ident;

    let gen = quote! {
        impl CommandDescriptor for #name {
            fn base_offset() -> usize {
                #base_offset
            }

            fn report_id() -> u8 {
                #report_id
            }

            fn cmd_len() -> usize {
                #cmd_len
            }
        }
    };

    TokenStream::from(gen)
}

#[proc_macro_attribute]
pub fn command_extension(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemImpl);

    let target_type = &*input.self_ty;

    // Expect target_type looks like Command<Something>
    let (_, generic_arg_type) = match target_type {
        Type::Path(TypePath { path, .. }) => {
            let first_segment = path.segments.first().expect("Expected a path segment");
            if first_segment.ident != "Command" {
                return syn::Error::new_spanned(
                    target_type,
                    "command_extension only works on impl blocks for Command<T>",
                )
                .to_compile_error()
                .into();
            }
            // Extract the single generic argument T from Command<T>.
            match &first_segment.arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                    if args.len() != 1 {
                        return syn::Error::new_spanned(
                            &first_segment.arguments,
                            "Expected exactly one generic argument",
                        )
                        .to_compile_error()
                        .into();
                    }
                    let generic_arg = args.first().unwrap();
                    match generic_arg {
                        GenericArgument::Type(ty) => (first_segment.ident.clone(), ty.clone()),
                        _ => {
                            return syn::Error::new_spanned(
                                generic_arg,
                                "Expected a type as the generic argument",
                            )
                            .to_compile_error()
                            .into();
                        }
                    }
                }
                _ => {
                    return syn::Error::new_spanned(
                        &first_segment.arguments,
                        "Expected angle bracketed generic arguments",
                    )
                    .to_compile_error()
                    .into();
                }
            }
        }
        _ => {
            return syn::Error::new_spanned(
                target_type,
                "command_extension can only be applied to impl blocks for Command<T>",
            )
            .to_compile_error()
            .into();
        }
    };

    let inner_type_ident = match generic_arg_type {
        Type::Path(TypePath { ref path, .. }) => path
            .segments
            .last()
            .expect("Expected at least one segment in the generic type")
            .ident
            .clone(),
        _ => {
            return syn::Error::new_spanned(
                generic_arg_type,
                "Expected a simple type for the generic parameter",
            )
            .to_compile_error()
            .into();
        }
    };

    let trait_name_str = format!("{}Ext", inner_type_ident);
    let trait_ident = Ident::new(&trait_name_str, proc_macro2::Span::call_site());

    let mut trait_methods = Vec::new();
    let mut impl_methods = Vec::new();

    for item in input.items.iter() {
        if let ImplItem::Fn(method) = item {
            let sig = &method.sig;
            let attrs = &method.attrs;
            let trait_method = quote! {
                #(#attrs)*
                #sig;
            };
            trait_methods.push(trait_method);

            let block = &method.block;
            let impl_method = quote! {
                #(#attrs)*
                #sig #block
            };
            impl_methods.push(impl_method);
        }
    }

    let trait_def = quote! {
        pub trait #trait_ident {
            #(#trait_methods)*
        }
    };

    let impl_block = quote! {
        impl #trait_ident for #target_type {
            #(#impl_methods)*
        }
    };

    let output = quote! {
        #trait_def
        #impl_block
    };

    output.into()
}
