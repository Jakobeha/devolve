use proc_macro::{Diagnostic, Level};
use std::iter::zip;
use proc_macro2::{Ident, TokenStream};
use syn::parse::{Parse, ParseStream};
use quote::{quote, ToTokens};
use syn::{Expr, FnArg, Item, ItemFn, LitStr, parenthesized, parse_quote, ReturnType, Stmt, Token, Type, TypeReference};
use syn::spanned::Spanned;

pub(crate) struct NodeTypeFn {
    attr: HeadAttr,
    ctx_arg: CtxArg,
    inputs: Vec<IOArg>,
    // TODO: Use validators
    #[allow(unused)] // See TODO
    validators: Vec<ValidatorFn>,
    outputs: Vec<IOArg>,
    fun: ItemFn,
}

#[derive(Default)]
struct FullHeadAttr {
    head: HeadAttr,
    return_attrs: Option<Vec<ArgAttr>>
}

#[derive(Default)]
struct HeadAttr {
    no_ctx: bool,
}

struct CtxArg {
    type_: TypeReference
}

struct IOArg {
    attr: ArgAttr,
    name: String,
    type_: IOArgType
}

struct IOArgType {
    type_: Type,
    is_reference: bool,
}

#[derive(Clone, Default)]
struct ArgAttr {
    name: Option<String>,
    default: Option<(Option<Token![=]>, Expr)>
}

struct ValidatorFn {
    #[allow(unused)] // See TODO
    fun: ItemFn
}

impl Parse for NodeTypeFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut fun = input.call(ItemFn::parse)?;
        let FullHeadAttr { head: attr, return_attrs } = fun.attrs.iter()
            .find(|attr| attr.path.is_ident("node_type"))
            .ok_or(syn::Error::new(fun.span(), "missing #[node_type] attribute"))?
            .parse_args::<FullHeadAttr>()?;
        let (ctx_arg, inputs) = if attr.no_ctx {
            (CtxArg::default(), fun.sig.inputs.iter().map(IOArg::try_from_arg).try_collect::<Vec<_>>()?)
        } else {
            (CtxArg::try_new(fun.sig.inputs.first())?, fun.sig.inputs.iter().skip(1).map(IOArg::try_from_arg).try_collect::<Vec<_>>()?)
        };
        let validators = fun.block.stmts.drain_filter(|stmt| match stmt {
            Stmt::Item(Item::Fn(fun)) => fun.attrs.iter().any(|attr| attr.path.is_ident("node_type") && attr.tokens.to_string() == "(validator)"),
            _ => false,
        }).map(|stmt| match stmt {
            Stmt::Item(Item::Fn(fun)) => Ok::<ValidatorFn, syn::Error>(ValidatorFn { fun }),
            _ => unreachable!(),
        }).try_collect::<Vec<_>>()?;
        let outputs = IOArg::try_from_output(&fun.sig.output, return_attrs)?;
        Ok(NodeTypeFn {
            attr,
            ctx_arg,
            inputs,
            validators,
            outputs,
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

        match arg.pat.as_ref() {
            syn::Pat::Ident(name) if name.ident == "ctx" || name.ident == "_ctx" => {},
            _pat =>
            // TODO: use pat.span(), the issue is that it is proc_macro2::Span and I don't know how to convert into proc_macro::Span
            //   also, proc_macro2 doesn't have Diagnostic (if it did I would use that)
                Diagnostic::new(Level::Warning, "context argument should be named `ctx`")
                    .help("add the no_ctx attribute to the function if you don't intend to have a ctx argument")
                    .help("rename the argument to `ctx`")
                    .emit()
        };

        let type_ = match &*arg.ty {
            Type::Reference(type_) => type_.clone(),
            _ => return Err(syn::Error::new(arg.ty.span(), "context argument must be a reference")),
        };

        Ok(Self { type_ })
    }
}

impl IOArg {
    fn try_from_arg(arg: &FnArg) -> syn::Result<Self> {
        let arg = match arg {
            FnArg::Receiver(arg) => return Err(syn::Error::new(arg.span(), "receiver arguments are not supported")),
            FnArg::Typed(arg) => arg,
        };

        let attr = arg.attrs.iter()
            .find(|attr| attr.path.is_ident("node_type"))
            .map(|attr| attr.parse_args::<ArgAttr>())
            .unwrap_or(Ok(ArgAttr::default()))?;

        let name = match &attr.name {
            None => match arg.pat.as_ref() {
                syn::Pat::Ident(name) => name,
                _ => return Err(syn::Error::new_spanned(&arg.pat, "argument can't be patterns without #[node_type(name = \"...\")]")),
            }.ident.to_string(),
            Some(name) => name.clone()
        };

        Self::try_new(attr, name, arg.ty.as_ref())
    }

    fn try_from_output(output: &ReturnType, return_attrs: Option<Vec<ArgAttr>>) -> syn::Result<Vec<Self>> {
        match output {
            ReturnType::Default => Err(syn::Error::new_spanned(output, "must provide explicit return type")),
            ReturnType::Type(_, ty) => {
                let return_attrs = Self::check_num_attrs(ty.as_ref(), return_attrs)?;
                Self::try_from_return_ty(ty.as_ref(), return_attrs)
            }
        }
    }

    fn check_num_attrs(ty: &Type, return_attrs: Option<Vec<ArgAttr>>) -> syn::Result<Vec<ArgAttr>> {
        let num_expected_attrs = Self::num_expected_attrs(ty);
        match return_attrs {
            None => Ok(vec![ArgAttr::default(); num_expected_attrs]),
            Some(return_attrs) => {
                if num_expected_attrs == return_attrs.len() {
                    Ok(return_attrs)
                } else {
                    Err(syn::Error::new_spanned(ty, format!("expected {} return attributes, got {}", num_expected_attrs, return_attrs.len())))
                }
            }
        }
    }

    fn num_expected_attrs(ty: &Type) -> usize {
        match ty {
            Type::Tuple(tuple) => tuple.elems.len(),
            _ => 1
        }
    }

    fn try_from_return_ty(ty: &Type, return_attrs: Vec<ArgAttr>) -> syn::Result<Vec<Self>> {
        match ty {
            Type::Tuple(tuple) => zip(return_attrs, &tuple.elems).enumerate()
                .map(|(index, (attr, ty))| IOArg::try_from_return_ty_part(attr, index, ty)).try_collect::<Vec<_>>(),
            ty => {
                let return_attr = return_attrs.into_iter().next().unwrap();
                Ok(vec![IOArg::try_from_return_ty_part(return_attr, 0, ty)?])
            }
        }
    }

    fn try_from_return_ty_part(attr: ArgAttr, index: usize, ty: &Type) -> syn::Result<Self> {
        let name = match &attr.name {
            None => match index {
                0 => "self".to_string(),
                _ => return Err(syn::Error::new_spanned(ty, "tuple return types must have names")),
            }
            Some(name) => name.clone()
        };
        Self::try_new(attr, name, ty)
    }

    fn try_new(attr: ArgAttr, name: String, ty: &Type) -> syn::Result<Self> {
        let type_ = IOArgType::from(ty.clone());
        Ok(Self { attr, name, type_ })
    }
}

impl From<Type> for IOArgType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Reference(TypeReference { elem, .. }) if ty.to_token_stream().to_string() != "&str" => IOArgType {
                type_: *elem,
                is_reference: true,
            },
            ty => IOArgType {
                type_: ty,
                is_reference: false,
            }
        }
    }
}

impl Parse for FullHeadAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut this = Self::default();
        while !input.is_empty() {
            let ident = input.parse::<Ident>()?;
            match ident.to_string().as_str() {
                "no_ctx" => this.head.no_ctx = true,
                "return" => {
                    let content;
                    parenthesized!(content in input);
                    this.return_attrs = Some(Vec::from_iter(
                        content.parse_terminated::<ArgAttr, Token![,]>(ArgAttr::parse)?
                    ));
                }
                _ => return Err(syn::Error::new(ident.span(), "unknown #[node_type] attribute"))
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        Ok(this)
    }
}

impl Parse for ArgAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut this = Self::default();
        while !input.is_empty() {
            let ident = input.parse::<Ident>()?;
            match ident.to_string().as_str() {
                "name" => {
                    input.parse::<Token![=]>()?;
                    this.name = Some(input.parse::<LitStr>()?.value());
                }
                "default" => {
                    this.default = Some(if input.peek(Token![=]) {
                        (
                            Some(input.parse::<Token![=]>()?),
                            input.parse::<LitStr>()?.parse::<Expr>()?
                        )
                    } else {
                        (None, parse_quote! { Default::default() })
                    })
                }
                _ => return Err(syn::Error::new(ident.span(), "unknown #[node_type] attribute"))
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
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
        let input_names = self.inputs.iter().map(|input|
            input.attr.name.as_ref().unwrap_or(&input.name)
        );
        let input_tys = self.inputs.iter().map(|input| &input.type_.type_);
        let input_nullabilitys = self.inputs.iter().map(|_|
            quote!(devolve::raw::nullability::NullRegion::NonNull)
        );
        let input_nullabilitys2 = self.inputs.iter().map(|_|
            quote!(devolve::raw::data::NonNull)
        );
        let input_defaults = self.inputs.iter().map(|input|
            input.attr.default.as_ref().map(|(_, expr)| expr)
        );
        let input_defaults2 = self.inputs.iter().map(|input|
            input.attr.default.as_ref().map(|(_, expr)| {
                let type_ = input.type_.type_.clone();
                quote!(NodeIO::inline_const::<#type_>(#expr))
            })
        );
        let inputs = self.inputs.iter().enumerate().map(|(index, input)| {
            if input.type_.is_reference {
                quote!(&inputs.#index)
            } else {
                quote!(inputs.#index)
            }
        });
        let output_names = self.outputs.iter().map(|output|
            output.attr.name.as_ref().unwrap_or(&output.name)
        );
        let output_tys = self.outputs.iter().map(|output| &output.type_.type_);
        let output_nullabilitys = self.outputs.iter().map(|_|
            quote!(devolve::raw::nullability::NullRegion::NonNull)
        );
        let output_nullabilitys2 = self.outputs.iter().map(|_|
            quote!(devolve::raw::data::NonNull)
        );
        let output_defaults = self.outputs.iter().map(|output|
            output.attr.default.as_ref().map(|(_, expr)| expr)
        );
        let output_defaults2 = output_defaults.clone();
        let input_tys2 = input_tys.clone();
        let output_tys2 = output_tys.clone();
        let fn_ctx = if self.attr.no_ctx {
            None
        } else {
            Some(quote!(fn_ctx))
        };
        quote! {
            // Must match the signature of NodeTypeFn
            pub fn #node_type_name(type_arg: &str, fn_ctx: ::devolve::raw::NodeTypeFnCtx<'_>) -> Result<::devolve::raw::NodeType<#ctx_ty>, Box<dyn ::std::error::Error>> + Send + Sync> {
                use structural_reflection::c_tuple::*;

                ::devolve::raw::NodeType {
                    compute: ComputeFn::new(|ctx, input_data, output_data| {
                        let input_data = unsafe { input_data.as_raw() };
                        let output_data = unsafe { output_data.as_raw() };
                        let inputs = unsafe { input_data.load::<CTuple!(
                            #(#input_nullabilitys2<#input_tys>),*
                        )>() }.into_trailing((#(#input_defaults),*));
                        let output = Fn::call(#fun_name(), (
                            #(#fn_ctx, )?
                            #(#inputs,)*
                        ));
                        unsafe { output_data.store(IODataTypes::from_reg(output, #(#output_defaults),*) as CTuple!(
                            #(#output_nullabilitys2<#output_tys>),*
                        )) }
                    }),
                    type_data: ::devolve::ir::NodeTypeData {
                        inputs: vec![
                            #(NodeIOType {
                                name: #input_names,
                                rust_type: #input_tys2,
                                nullability: #input_nullabilitys
                            }),*
                        ]
                        outputs: vec![
                            #(NodeIOType {
                                name: #output_names,
                                rust_type: #output_tys2,
                                nullability: #output_nullabilitys
                            }),*
                        ]
                    },
                    default_inputs: vec![
                        #(#input_defaults2),*
                    ],
                    default_default_outputs: vec![
                        #(#output_defaults2),*
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

impl Default for CtxArg {
    fn default() -> Self {
        CtxArg {
            type_: parse_quote! { &mut () }
        }
    }
}