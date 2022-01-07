use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields, FieldsNamed};

const VEC_TYPE_NAME: &str = "Vec";
const OPTION_TYPE_NAME: &str = "Option";

fn get_inner_type<'a, 'b>(outer_type_name: &'a str, ty: &'b syn::Type) -> Option<&'b syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1
            || p.path.segments.iter().last().unwrap().ident != outer_type_name
        {
            return std::option::Option::None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_type) = p.path.segments[0].arguments {
            if inner_type.args.len() != 1 {
                return std::option::Option::None;
            }

            let inner_type = inner_type.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_type {
                return std::option::Option::Some(t);
            }
        }
    }
    std::option::Option::None
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
        if get_inner_type(OPTION_TYPE_NAME, ty).is_some()
            || get_inner_type(VEC_TYPE_NAME, ty).is_some()
        {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if get_inner_type(OPTION_TYPE_NAME, ty).is_some()
            || get_inner_type(VEC_TYPE_NAME, ty).is_some()
        {
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
        let ty = &f.ty;
        if get_inner_type(VEC_TYPE_NAME, ty).is_some() {
            quote! {
                #name: Vec::new()
            }
        } else {
            quote! {
                #name: std::option::Option::None
            }
        }
    });

    let author_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let std::option::Option::Some(inner_ty) = get_inner_type(OPTION_TYPE_NAME, ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty)->&mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else if let std::option::Option::Some(_) = get_inner_type(VEC_TYPE_NAME, ty) {
            quote! {
                pub fn #name(&mut self, #name: #ty)->&mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty)->&mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        }
    });

    fn mk_err<T: quote::ToTokens>(t: T) -> Option<proc_macro2::TokenStream> {
        std::option::Option::Some(
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        )
    }
    let each_author_methods = fields.iter().map(|f| {
        for attrs in &f.attrs {
            if attrs.path.segments.len() != 1 {
                return std::option::Option::None;
            }

            if !attrs.path.is_ident("builder") {
                return std::option::Option::None;
            }

            let attr_list = match attrs.parse_meta() {
                Ok(syn::Meta::List(l)) => l,
                Ok(x) => return mk_err(x),
                Err(e) => return std::option::Option::Some(e.to_compile_error()),
            };

            let each_nv = attr_list.nested.pairs().find(|p| {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = p.value() {
                    return nv.path.is_ident("each");
                } else {
                    false
                }
            });

            if let std::option::Option::None = each_nv {
                return mk_err(attr_list);
            }

            let each_lit = match each_nv.unwrap().value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => nv.lit.clone(),
                meta => return mk_err(meta),
            };

            let each_name = match each_lit {
                syn::Lit::Str(ref s) => s.value(),
                _ => return mk_err(attr_list),
            };

            let name = syn::Ident::new(each_name.as_str(), each_lit.span());
            let field_name = f.ident.as_ref().unwrap();
            if name.to_string() == field_name.to_string() {
                return std::option::Option::None;
            }

            let inner_ty = get_inner_type(VEC_TYPE_NAME, &f.ty);
            return std::option::Option::Some(quote! {
                pub fn #name(&mut self, #name: #inner_ty)->&mut Self {
                    self.#field_name.push(#name);
                    self
                }
            });
        }
        std::option::Option::None
    });
    let expanded = quote! {
        struct #bident {
            #(#optionised_fields,)*
        }

        impl #bident {
            #(#each_author_methods)*
            #(#author_methods)*
            pub fn build(&self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
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
