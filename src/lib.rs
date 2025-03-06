use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, FnArg, GenericArgument, Ident,
    ImplItem, ItemImpl, Pat, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Command)]
pub fn command_trait(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;
    quote! {
        impl CommandDescriptor for #name {}
    }
    .into()
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
