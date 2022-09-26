use proc_macro2::TokenStream;
use test;

#[proc_macro]
pub fn extract_pattern(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    TokenStream::new().into()
}
