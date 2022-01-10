use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Token};

#[derive(Debug)]
struct SequenceMacroInput {
    range: syn::ExprRange,
    body: proc_macro2::TokenStream,
}

impl Parse for SequenceMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::Ident>()?;
        input.parse::<Token![in]>()?;
        Ok(SequenceMacroInput {
            range: input.parse::<syn::ExprRange>()?,
            body: proc_macro2::TokenStream::parse(input)?,
        })
    }
}

// impl From<SequenceMacroInput> for TokenStream {}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SequenceMacroInput);
    eprintln!("{:#?}", ast);
    let expanded = quote! { /****/};
    expanded.into()
}
