use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    AngleBracketedGenericArguments, DeriveInput, Expr, ExprLit, FnArg, GenericArgument, Ident,
    ImplItem, ItemImpl, Lit, MetaNameValue, Pat, PathArguments, Result, Token, Type, TypePath,
};

/// Struct for holding the parsed attribute arguments.
struct Attributes {
    report_id: u8,
    cmd_len: usize,
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse the comma-separated list of key = value pairs.
        let args = Punctuated::<MetaNameValue, Token![,]>::parse_terminated(input)?;
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
        let report_id =
            report_id_opt.ok_or_else(|| syn::Error::new(input.span(), "Missing `report_id`"))?;
        let cmd_len =
            cmd_len_opt.ok_or_else(|| syn::Error::new(input.span(), "Missing `cmd_len`"))?;

        Ok(Attributes { report_id, cmd_len })
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
    let report_id = args.report_id;
    let cmd_len = args.cmd_len;

    let name = &ast.ident;

    let gen = quote! {
        impl CommandDescriptor for #name {
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

    // Build names for the two traits:
    // For Command<T> extension trait (the original full set, including both getters and setters).
    let trait_name_str = format!("{}Ext", inner_type_ident);
    let trait_ident = Ident::new(&trait_name_str, proc_macro2::Span::call_site());
    // For the builder extension trait (only for setters). We’ll call it CommandNameBuilderExt.
    let builder_trait_name_str = format!("{}BuilderExt", inner_type_ident);
    let builder_trait_ident = Ident::new(&builder_trait_name_str, proc_macro2::Span::call_site());

    // Prepare to collect the methods for the two generated impls.
    let mut cmd_trait_methods = Vec::new();
    let mut cmd_impl_methods = Vec::new();

    let mut builder_trait_methods = Vec::new();
    let mut builder_impl_methods = Vec::new();

    // For each function in the input impl block…
    for item in input.items.iter() {
        if let ImplItem::Fn(method) = item {
            let sig = &method.sig;
            let attrs = &method.attrs;

            // Add the method to the command trait (the “full” trait) as-is.
            let trait_method = quote! {
                #(#attrs)*
                #sig;
            };
            cmd_trait_methods.push(trait_method);

            let block = &method.block;
            let impl_method = quote! {
                #(#attrs)*
                #sig #block
            };
            cmd_impl_methods.push(impl_method);

            // Now if the method name begins with "set_", generate the corresponding builder method.
            let method_name = sig.ident.to_string();
            if let Some(stripped) = method_name.strip_prefix("set_") {
                // New (builder) method name: drop the set_ prefix.
                let builder_method_ident = Ident::new(stripped, sig.ident.span());

                // The builder method’s signature becomes:
                //     fn <builder_method_ident>(mut self, <args from the original> ) -> Self;
                // The original setter is assumed to have a receiver (e.g. &mut self) plus one or more parameters.
                // We remove the original receiver and use "mut self" instead.
                let mut builder_inputs = Vec::new();
                let mut arg_idents = Vec::new();
                // iterate over inputs skipping the receiver.
                for input in sig.inputs.iter().skip(1) {
                    builder_inputs.push(input);
                    // Also extract the identifier from each argument so we can pass it on.
                    if let FnArg::Typed(pat_type) = input {
                        // We expect the pat to be a simple identifier.
                        if let Pat::Ident(pat_ident) = *pat_type.pat.clone() {
                            arg_idents.push(pat_ident.ident);
                        }
                    }
                }

                // Build the builder method signature. We want something like:
                //    fn rgb_lighting_effects(mut self, <params>) -> Self;
                let builder_sig = quote! {
                    fn #builder_method_ident(self, #(#builder_inputs),* ) -> Self;
                };

                builder_trait_methods.push(builder_sig);

                // Inside the loop over methods, before generating the builder impl:
                let setter_ident = sig.ident.clone();

                // Then generate the builder impl using the captured setter_ident:
                let builder_impl = quote! {
                    fn #builder_method_ident(mut self, #(#builder_inputs),* ) -> Self {
                        self.command.#setter_ident( #(#arg_idents),* );
                        self
                    }
                };

                builder_impl_methods.push(builder_impl);
            }
        }
    }

    // Build the command extension trait definition and its impl block.
    let cmd_trait_def = quote! {
        pub trait #trait_ident {
            #(#cmd_trait_methods)*
        }
    };

    let cmd_impl_block = quote! {
        impl #trait_ident for #target_type {
            #(#cmd_impl_methods)*
        }
    };

    // Build the builder extension trait definition.
    let builder_trait_def = quote! {
        pub trait #builder_trait_ident {
            #(#builder_trait_methods)*
        }
    };

    // Build the builder target type: CommandBuilder<T>
    let builder_target = quote! { CommandBuilder<#generic_arg_type> };

    let builder_impl_block = quote! {
        impl #builder_trait_ident for #builder_target {
            #(#builder_impl_methods)*
        }
    };

    // Finally, put everything together. The output contains:
    //  - the original command extension trait and impl for Command<T>
    //  - the builder extension trait and impl for CommandBuilder<T>
    let output = quote! {
        #cmd_trait_def
        #cmd_impl_block

        #builder_trait_def
        #builder_impl_block
    };

    output.into()
}
