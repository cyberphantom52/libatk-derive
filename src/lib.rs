use proc_macro::TokenStream;

use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, DeriveInput, Result, Token};
use syn::{Expr, ExprLit, Lit, MetaNameValue};

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
