use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};
use quote::ToTokens;

struct NodeTypeAttribute;

impl Parse for NodeTypeAttribute {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

impl ToTokens for NodeTypeAttribute {
    fn to_tokens(&self, _tokens: &mut TokenStream) {
        todo!()
    }
}