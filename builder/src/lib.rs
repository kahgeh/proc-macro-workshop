use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::ParseStream, parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields,
    FieldsNamed, Token,
};

fn get_inner_type<'a, 'b>(outer_type_name: &'a str, ty: &'b syn::Type) -> Option<&'b syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1
            || p.path.segments.iter().last().unwrap().ident != outer_type_name
        {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_type) = p.path.segments[0].arguments {
            if inner_type.args.len() != 1 {
                return None;
            }

            let inner_type = inner_type.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_type {
                return Some(t);
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
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
        if get_inner_type("Option", ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let author_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = get_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty)->&mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty)->&mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if get_inner_type("Option", ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;

        quote! {
            #name: None
        }
    });
    let each_author_methods = fields.iter().map(|f| {
        for attrs in &f.attrs {
            if attrs.path.segments.len() != 1 {
                return None;
            }

            if !attrs.path.is_ident("builder") {
                return None;
            }
            match attrs.parse_args_with(|input: ParseStream| {
                match input.parse::<syn::Ident>() {
                    Ok(ident) => {
                        assert_eq!(ident, "each");
                        Ok(ident)
                    }
                    Err(e) => Err(e),
                }?;
                input.parse::<Token![=]>()?;
                input.parse::<syn::LitStr>()
            }) {
                Ok(name_lit) => {
                    //return Some(quote! { /****/});
                    let name = syn::Ident::new(name_lit.value().as_str(), name_lit.span());
                    return Some(quote! { pub fn #name(&mut self)->&mut Self {} });
                }
                Err(err) => panic!("expected each=name, {:?}", err),
            }
        }
        None
    });
    let expanded = quote! {
        struct #bident {
            #(#optionised_fields,)*
        }

        impl #bident {
            #(#each_author_methods)*
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
