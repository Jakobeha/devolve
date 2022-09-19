/// `graph!(path = ..., comptime_ctx = ... | fn ...)`:
/// Generates a function which computes itself using a graph at the specified path, with lazy-load and reload using modify dates.
/// Path is relative to the current file unless absolute or starts with `$CARGO_MANIFEST_DIR`.
/// `comptime_ctx` should be a `const` variable, as it will be a static reference.
///
/// `graph!(graph = ..., input_data = ..., output_data = ... | fn ...)`: Generates a function which computes itself using the specified graph, which should be a `SelfContainedRuntimeGraph`.
///
/// `graph!(graph_resolver = ... | fn ...)`: Generates a function which computes itself by calling `.resolve` on the expression passed to `graph_resolver`.
///
/// You can provide multiple functions if the graph takes default arguments.
/// Arguments must correspond to input names, and any missing arguments are substituted with their default values.
/// If `ctx = ... |` is provided, the first argument of the function will be `&mut ctx` and that will be the `RuntimeCtx`. Otherwise `RuntimeCtx` must be `()`.
/// Inputs and outputs will be checked at runtime unless the function is marked `unsafe`. `path = ...` and custom graph_resolvers can cache if inputs/outputs were cached for a function so they are not checked redundantly.
/// It is discouraged to have a safe function which calls without checking: currently if you want
/// this, you will have to write your own wrapper around the `unsafe` generated one.
///
/// # Example
///
/// ```rust
/// use dui_graph::macros::graph;
///
/// struct MyRuntimeCtx;
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// struct Vector3<T> { x: T, y: T, z: T }
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// enum DistanceType { Euclidean, Manhattan }
///
/// graph! { path = "max_distance.dui", ctx = MyRuntimeCtx |
///     pub fn max_euclidean_distance(inputs: &[Vector3<f32>]) -> (f32);
///     pub fn max_distance(inputs: &[Vector3<f32>], distance_type: DistanceType) -> (f32);
///     pub unsafe fn max_distance_unsafe(inputs: &[Vector3<f32>], distance_type: DistanceType) -> (f32);
/// };
/// ```
pub macro graph {
{ graph_resolver = $graph_resolver:expr $(, ctx = $ctx_ty:ty)? |
      $($vis:vis $( $unsafe:tt )? fn $name:ident($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*));* $(;)?
    } => {
        $(
            #[allow(unsafe_block_in_unsafe_fn)]
            $vis $( $unsafe )? fn $name($(ctx: &mut $ctx_ty, )? $($arg: $arg_ty),*) -> ($($ret_ty),*) {
                $graph_resolver.with_resolve(stringify!($name), |(graph, inputs, outputs, is_checked)| {
                    let inputs = inputs.subset(&[$(stringify!($arg)),*]);
                    __first!($( {} __ignore!($unsafe) )?, if !is_checked {
                        inputs.check::<::structural_reflection::c_tuple::CTuple!($($arg_ty),*)>();
                        outputs.check::<::structural_reflection::c_tuple::CTuple!($($ret_ty),*)>();
                    })
                    unsafe { inputs.store(::structural_reflection::c_tuple::c_tuple!($($arg),*)) };
                    graph.run(&mut __first!($( ctx __ignore($ctx_ty) )?, ()), unsafe { inputs.as_input() }, unsafe { outputs.as_output() });
                    unsafe { outputs.load::<::structural_reflection::c_tuple::CTuple!($($ret_ty),*)>() }.into()
                })
            }
        )*
    },
{ graph = $graph:expr, $(, $($tt1:tt)*)? | $($tt2:tt)* } => {
        graph!(graph_resolver = ::dui_graph::macros::graph_resolvers::IdentityGraphResolver($graph) $(, $($tt1)*)? | $($tt2)*)
    },
{ path = $path:literal, comptime_ctx = $comptime_ctx:expr $(, ctx = $ctx_ty:ty $( , $($tt1:tt)*)?)? | $($tt2:tt)*  } => {
        lazy_static::lazy_static! {
            static ref PATH_RESOLVER: PathGraphResolver<'static, __first!($( $ctx_ty )?, ())> = ::dui_graph::macros::graph_resolvers::PathGraphResolver::new(
                ::dui::macros::graph_resolvers::resolve_graph_path($path, ::std::path::Path(env!("CARGO_MANIFEST_DIR"), file!())),
                &$comptime_ctx
            );
        }

        graph!(graph_resolver = PATH_RESOLVER $(, ctx = $ctx_ty $(, $($tt1)*)?)? | $($tt2)*)
    }
}

macro __ignore($(tt:tt)*) {}

macro __first($first:tt $(, $rest:tt)*) {
$first
}