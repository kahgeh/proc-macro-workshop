use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Token};

#[derive(Debug)]
struct SequenceMacroInput {}

impl Parse for SequenceMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::Ident>()?;
        input.parse::<Token![in]>()?;
        input.parse::<syn::Lit>()?;
        input.parse::<Token![..]>()?;
        input.parse::<syn::Lit>()?;
        input.parse::<syn::ExprBlock>()?;
        Ok(SequenceMacroInput {})
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SequenceMacroInput);
    eprintln!("{:#?}", ast);
    let expanded = quote! { /****/};
    expanded.into()
}
