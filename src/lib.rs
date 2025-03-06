use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, DeriveInput, FnArg, GenericArgument, ImplItem, ItemImpl, Pat, PathArguments,
    Type, TypePath,
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

fn get_inner_type(ty: &syn::Type) -> syn::Result<syn::Type> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            if segment.ident != "Command" {
                return Err(syn::Error::new_spanned(
                    ty.to_token_stream(),
                    "#[command] only works on impl blocks for Command<T>",
                ));
            }
            match &segment.arguments {
                PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args, ..
                }) => {
                    if args.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            &segment.arguments,
                            "Expected exactly one generic argument",
                        ));
                    }
                    let arg = args.first().unwrap();
                    return match arg {
                        GenericArgument::Type(t) => Ok(t.clone()),
                        _ => Err(syn::Error::new_spanned(
                            arg,
                            "Expected a type as the generic argument",
                        )),
                    };
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        &segment.arguments,
                        "Expected angle bracketed generic arguments",
                    ))
                }
            }
        }
        _ => Err(syn::Error::new_spanned(
            &ty.to_token_stream(),
            "#[command] only works on impl blocks for Command<T>",
        )),
    }
}

fn get_ident_from_type(ty: &syn::Type) -> syn::Result<syn::Ident> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last();
            match segment {
                Some(seg) => Ok(seg.ident.clone()),
                None => Err(syn::Error::new_spanned(
                    ty,
                    "Expected at least one segment in the generic type",
                )),
            }
        }
        _ => Err(syn::Error::new_spanned(
            ty,
            "Expected a simple type for the generic parameter",
        )),
    }
}

#[proc_macro_attribute]
pub fn command_extension(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemImpl);
    let impl_ty = input.self_ty;
    let inner_ty = match get_inner_type(&impl_ty) {
        Ok(ty) => ty,
        Err(e) => return e.to_compile_error().into(),
    };

    let inner_ident = match get_ident_from_type(&inner_ty) {
        Ok(ident) => ident,
        Err(e) => return e.to_compile_error().into(),
    };

    let extension_trait_ident = syn::Ident::new(&format!("{}Ext", inner_ident), inner_ident.span());
    let mut extension_trait_methods = Vec::new();
    let mut extension_impl_methods = Vec::new();

    let builder_trait_ident =
        syn::Ident::new(&format!("{}BuilderExt", inner_ident), inner_ident.span());
    let mut builder_trait_methods = Vec::new();
    let mut builder_trait_impl = Vec::new();

    for item in input.items.iter() {
        if let ImplItem::Fn(method) = item {
            let sig = &method.sig;
            let attrs = &method.attrs;
            let block = &method.block;

            extension_trait_methods.push(quote! {
                #(#attrs)*
                #sig;
            });
            extension_impl_methods.push(quote! {
                #(#attrs)*
                #sig #block
            });

            let method_name = sig.ident.to_string();
            if method_name.starts_with("set_") {
                if sig.inputs.len() < 2 {
                    return syn::Error::new_spanned(
                        sig,
                        "Expected at least one argument for setter method",
                    )
                    .to_compile_error()
                    .into();
                }

                let new_method_name =
                    syn::Ident::new(method_name.strip_prefix("set_").unwrap(), sig.ident.span());
                let mut builder_inputs = Vec::new();
                let mut arg_idents = Vec::new();
                for input in sig.inputs.iter().skip(1) {
                    builder_inputs.push(input);
                    if let FnArg::Typed(pat_type) = input {
                        if let Pat::Ident(pat_ident) = *pat_type.pat.clone() {
                            arg_idents.push(pat_ident.ident);
                        }
                    }
                }

                let builder_sig = quote! {
                    fn #new_method_name(self, #(#builder_inputs),* ) -> Self;
                };
                builder_trait_methods.push(builder_sig);
                let setter_ident = &sig.ident;
                builder_trait_impl.push(quote! {
                    fn #new_method_name(mut self, #(#builder_inputs),* ) -> Self {
                        self.command.#setter_ident( #(#arg_idents),* );
                        self
                    }
                });
            }
        }
    }

    let mut out = quote! {
        pub trait #extension_trait_ident {
            #(#extension_trait_methods)*
        }

        impl #extension_trait_ident for #impl_ty {
            #(#extension_impl_methods)*
        }
    };

    if builder_trait_methods.len() > 0 {
        out.extend(quote! {
            pub trait #builder_trait_ident {
                #(#builder_trait_methods)*
            }

            impl #builder_trait_ident for CommandBuilder<#inner_ident> {
                #(#builder_trait_impl)*
            }
        });
    }
    out.into()
}
