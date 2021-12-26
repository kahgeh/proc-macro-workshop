use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields, FieldsNamed};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    eprintln!("tokens: {}", input);
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    eprintln!("{:#?}", ast);
    let bname = format!("{}Builder", ast.ident);
    let bident = syn::Ident::new(&bname[..], name.span());
    let fields = if let Struct(
        DataStruct {
            fields: Fields::Named(FieldsNamed { ref named, .. }),
            ..
        },
        ..,
    ) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    let optionised_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! { #name: std::option::Option<#ty> }
    });

    let author_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        quote! {
            fn #name(&mut self, #name: #ty)->&mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;

        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;

        quote! {
            #name: None
        }
    });

    let expanded = quote! {
        struct #bident {
            #(#optionised_fields,)*
        }

        impl #bident {
            #(#author_methods)*
            pub fn build(&self) -> Result<Command, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

        impl #name {
            fn builder() ->#bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };
    expanded.into()
}
