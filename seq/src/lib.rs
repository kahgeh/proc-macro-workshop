use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced,
    parse::{Parse, Parser},
    parse_macro_input, Token,
};

#[derive(Debug)]
struct SequenceMacroInput {
    ident: syn::Ident,
    from: i64,
    to: i64,
    body: proc_macro2::TokenStream,
}

impl Parse for SequenceMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        input.parse::<Token![in]>()?;
        let from = input
            .parse::<syn::LitInt>()?
            .base10_digits()
            .parse()
            .unwrap();
        input.parse::<Token![..]>()?;
        let to = input
            .parse::<syn::LitInt>()?
            .base10_digits()
            .parse()
            .unwrap();
        let content;
        braced!(content in input);
        Ok(SequenceMacroInput {
            ident,
            from,
            to,
            body: content.parse()?,
        })
    }
}

impl SequenceMacroInput {
    fn expand(&self, _n: i64) -> TokenStream {
        let body = self.body.clone();
        (quote! {
            #body
        })
        .into()
    }
}

impl Into<TokenStream> for SequenceMacroInput {
    fn into(self) -> TokenStream {
        (self.from..self.to).map(|n| self.expand(n)).collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SequenceMacroInput);
    eprintln!("{:#?}", ast);
    ast.into()
}
