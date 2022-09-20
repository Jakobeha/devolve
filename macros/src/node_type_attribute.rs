use proc_macro::{Diagnostic, Level};
use std::intrinsics::arith_offset;
use std::iter::once;
use proc_macro2::{Ident, TokenStream};
use syn::parse::{Parse, ParseStream};
use quote::{quote, ToTokens};
use syn::{Attribute, Expr, FnArg, ItemFn, LitStr, parse_quote, Type, TypeReference};
use syn::spanned::Spanned;

pub(crate) struct NodeTypeFn {
    pub attr: HeadAttr,
    pub ctx_arg: CtxArg,
    pub input_args: Vec<InputArg>,
    pub validators: Vec<ValidatorFn>,
    pub fun: ItemFn,
}

#[derive(Default)]
struct HeadAttr {
    pub no_ctx: bool,
}

struct CtxArg {
    pub type_: TypeReference
}

struct InputArg {
    pub attr: ArgAttr,
    pub name: Ident,
    pub type_: InputArgType
}

enum InputArgType {
    VarLenArray { elem: Type },
    Type(Type),
}

#[derive(Default)]
struct ArgAttr {
    pub default: Option<(Option<syn::Token![=]>, Expr)>
}

struct ValidatorFn {
    fun: ItemFn
}

impl Parse for NodeTypeFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut fun = input.call(ItemFn::parse)?;
        let attr = fun.attrs.iter()
            .find(|attr| attr.path.is_ident("node_type"))
            .ok_or(syn::Error::new(fun.span(), "missing #[node_type] attribute (how?)"))?
            .parse_args::<HeadAttr>()?;
        let (ctx_arg, input_args) = if attr.no_ctx {
            (CtxArg::default(), fun.sig.inputs.iter().map(InputArg::try_new).try_collect::<Vec<_>>()?)
        } else {
            (CtxArg::try_new(fun.sig.inputs.first()), fun.sig.inputs.iter().skip(1).map(InputArg::try_new).try_collect::<Vec<_>>()?)
        };
        let validators = fun.block.stmts.drain_filter(|stmt| match stmt {
            syn::Stmt::Item(syn::Item::Fn(fun)) => fun.attrs.iter().any(|attr| attr.path.is_ident("node_type") && attr.tokens.to_string() == "(validator)"),
            _ => false,
        }).map(|stmt| match stmt {
            syn::Stmt::Item(syn::Item::Fn(fun)) => Ok(ValidatorFn { fun: fun }),
            _ => unreachable!(),
        }).try_collect::<Vec<_>>()?;
        Ok(NodeTypeFn {
            attr,
            ctx_arg,
            input_args,
            validators,
            fun,
        })
    }
}

impl CtxArg {
    fn try_new(arg: Option<&FnArg>) -> syn::Result<Self> {
        let arg = match arg {
            None => return Err(syn::Error::new(proc_macro2::Span::call_site(), "missing context argument")),
            Some(arg) => arg,
        };

        let arg = match arg {
            FnArg::Receiver(arg) => return Err(syn::Error::new(arg.span(), "receiver context argument is not supported")),
            FnArg::Typed(arg) => arg,
        };

        let name = &match &arg.pat {
            syn::Pat::Ident(name) => name,
            _ => return Err(syn::Error::new(arg.pat.span(), "only named context argument is supported, no patterns")),
        }.ident;

        if name != "ctx" && name != "_ctx" {
            Diagnostic::spanned(name.span(), Level::Warning, syn::Error::new(name.span(), "context argument should be named `ctx`"))
                .help("add the no_ctx attribute to the function if you don't intend to have a ctx argument")
                .help("rename the argument to `ctx`")
                .emit();
        }

        let type_ = match &*arg.ty {
            Type::Reference(type_) => type_.clone(),
            _ => return Err(syn::Error::new(arg.ty.span(), "context argument must be a reference")),
        };

        Ok(Self { type_ })
    }
}

impl InputArg {
    fn try_new(arg: &FnArg) -> syn::Result<Self> {
        let arg = match arg {
            FnArg::Receiver(arg) => return Err(syn::Error::new(arg.span(), "receiver arguments are not supported")),
            FnArg::Typed(arg) => arg,
        };

        let name = match &arg.pat {
            syn::Pat::Ident(name) => name,
            _ => return Err(syn::Error::new(arg.pat.span(), "only named arguments are supported, no patterns")),
        }.ident.clone();

        let attr = arg.attrs.iter()
            .find(|attr| attr.path.is_ident("node_type"))
            .map(|attr| attr.parse_args::<ArgAttr>())
            .unwrap_or(Ok(ArgAttr::default()))?;
        let type_ = match arg.ty.as_ref() {
            Type::Reference(Type::Slice(slice_ty)) => InputArgType::VarLenArray { elem: slice_ty.elem.as_ref().clone() },
            arg_ty => InputArgType::Type(arg_ty.clone()),
        };
        Ok(Self { attr, name, type_ })
    }
}

impl Parse for HeadAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut this = Self::default();
        while !input.is_empty() {
            let ident = input.parse::<syn::Ident>()?;
            match ident.to_string().as_str() {
                "no_ctx" => this.no_ctx = true,
                _ => return Err(syn::Error::new(ident.span(), "unknown #[node_type] attribute"))
            }
            if !input.is_empty() {
                input.parse::<syn::Token![,]>()?;
            }
        }
        Ok(this)
    }
}

impl Parse for ArgAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut this = Self::default();
        while !input.is_empty() {
            let ident = input.parse::<syn::Ident>()?;
            match ident.to_string().as_str() {
                "default" => {
                    this.default = Some(if input.peek(syn::Token![=]) {
                        (
                            Some(input.parse::<syn::Token![=]>()?),
                            input.parse::<LitStr>()?.parse::<Expr>()?
                        )
                    } else {
                        (None, parse_quote! { Default::default() })
                    })
                }
                _ => return Err(syn::Error::new(ident.span(), "unknown #[node_type] attribute"))
            }
            if !input.is_empty() {
                input.parse::<syn::Token![,]>()?;
            }
        }
        Ok(this)
    }
}

impl NodeTypeFn {
    fn node_type_name(&self) -> String {
        format!("{}__node_type", self.fun.sig.ident)
    }

    fn node_type_decl(&self) -> TokenStream {
        let fun_name = &self.fun.sig.ident;
        let node_type_name = self.node_type_name();
        let ctx_ty = &self.ctx_arg.type_.elem;
        quote! {
            // Must match the signature of NodeTypeFn
            pub fn #node_type_name(arg: &str, fn_ctx: ::dui_graph::node_types::NodeTypeFnCtx<'_>) -> Result<::dui_graph::node_types::NodeType<#ctx_ty>, Box<dyn ::std::error::Error>> + Send + Sync> {
                ::dui_graph::node_type::NodeType {
                    compute: |ctx, input_data, output_data| {

                    },
                    type_data: ::dui_graph::ir::NodeTypeData {
                        inputs: vec![
                            NodeIOType {
                                name:
                                rust_type:
                                null_region:
                            }
                        ]
                        outputs: vec![
                            NodeIOType {
                                name:
                                rust_type:
                                null_region:
                            }
                        ]
                    },
                    default_inputs: vec![

                    ],
                    default_default_outputs: vec![

                    ]
                }
            }
        }
    }
}

impl ToTokens for NodeTypeFn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.node_type_decl());
        tokens.extend(self.fun.to_token_stream());
    }
}