/// `graph!(path = ..., comptime_ctx = ...; fn ...)`:
/// Generates a function which computes itself using a graph at the specified path, with lazy-load and reload using modify dates.
/// Path is relative to the current file's parent directory unless absolute or starts with `$CARGO_MANIFEST_DIR`.
/// `comptime_ctx` should be a `const` variable, as it will be a static reference.
///
/// `graph!(graph = ..., input_data = ..., output_data = ...; fn ...)`: Generates a function which computes itself using the specified graph, which should be a `SelfContainedRuntimeGraph`.
///
/// `graph!(graph_resolver = ...; fn ...)`: Generates a function which computes itself by calling `.resolve` on the expression passed to `graph_resolver`.
///
/// You can provide multiple functions if the graph takes default arguments.
/// Arguments must correspond to input names, and any missing arguments are substituted with their default values.
/// If `ctx = ...;` is provided, the first argument of the function will be `&mut ctx` and that will be the `RuntimeCtx`. Otherwise `RuntimeCtx` must be `()`.
/// Inputs and outputs will be checked at runtime unless the function is marked `unsafe`. `path = ...` and custom graph_resolvers can cache if inputs/outputs were cached for a function so they are not checked redundantly.
/// It is discouraged to have a safe function which calls without checking: currently if you want
/// this, you will have to write your own wrapper around the `unsafe` generated one.
///
/// # Example
///
/// ```rust
/// use devolve::macros::graph;
///
/// struct MyRuntimeCtx;
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// struct Vector3<T> { x: T, y: T, z: T }
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// enum DistanceType { Euclidean, Manhattan }
///
/// graph! { path = "max_distance.dvl", comptime_ctx = MY_COMPTIME_CTX, ctx = MyRuntimeCtx;
///     pub fn max_euclidean_distance(inputs: &[Vector3<f32>]) -> (f32);
///     pub fn max_distance(inputs: &[Vector3<f32>], distance_type: DistanceType) -> (f32);
///     pub unsafe fn max_distance_unsafe(inputs: &[Vector3<f32>], distance_type: DistanceType) -> (f32);
/// }
/// ```
pub macro graph {
(2 _: $arg_ty:ty) => { $crate::graph::raw::Nullable::<$arg_ty>::None },
(2 $arg:ident: Option<$arg_ty:ty>) => { $crate::graph::raw::Nullable::<$arg_ty>::from_option($arg) },
(2 $arg:ident: $arg_ty:ty) => { $crate::graph::raw::Nullable::<$arg_ty>::Some($arg) },
(3 Option<$ret_ty:ty>) => { $crate::graph::raw::Nullable<$ret_ty> },
(3 $ret_ty:ty) => { $crate::graph::raw::NonNull<$ret_ty> },
{ 1 graph_resolver = $graph_resolver:expr, ctx = $ctx_ty:ty;
    $vis:vis unsafe fn $name:ident($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*)
} => {
    #[allow(unsafe_op_in_unsafe_fn, unused_parens, unused_imports)]
    $vis unsafe fn $name(ctx: &mut $ctx_ty, $($arg: $arg_ty),*) -> ($($ret_ty),*) {
        use $crate::macros::graph_resolvers::GraphResolver;
        use $crate::graph::raw::IODataTypes;
        use structural_reflection::c_tuple::{CTuple, c_tuple};

        $graph_resolver.with_resolve(stringify!($name), |graph, inputs, outputs, _is_checked| {
            unsafe {
                inputs.store_all_unchecked(c_tuple!($(graph!(2 $arg: $arg_ty)),*));
                graph.compute_unchecked(ctx, inputs.read_only(), outputs.write_only());
                outputs.load_all_unchecked::<CTuple!($(graph!(3 $ret_ty)),*)>().into_normal()
            }
        })
    }
},
{ 1 graph_resolver = $graph_resolver:expr;
    $vis:vis unsafe fn $name:ident($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*)
} => {
    #[allow(unsafe_op_in_unsafe_fn, unused_parens, unused_imports)]
    $vis unsafe fn $name($($arg: $arg_ty),*) -> ($($ret_ty),*) {
        use $crate::macros::graph_resolvers::GraphResolver;
        use $crate::graph::raw::IODataTypes;
        use structural_reflection::c_tuple::{CTuple, c_tuple};

        $graph_resolver.with_resolve(stringify!($name), |graph, inputs, outputs, _is_checked| {
            unsafe {
                inputs.store_all_unchecked(c_tuple!($(graph!(2 $arg: $arg_ty)),*));
                graph.compute_unchecked(&mut (), inputs.read_only(), outputs.write_only());
                outputs.load_all_unchecked::<CTuple!($(graph!(3 $ret_ty)),*)>().into_normal()
            }
        })
    }
},
{ 1 graph_resolver = $graph_resolver:expr, ctx = $ctx_ty:ty;
    $vis:vis fn $name:ident($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*)
} => {
    #[allow(unsafe_op_in_unsafe_fn, unused_parens, unused_imports)]
    $vis fn $name(ctx: &mut $ctx_ty, $($arg: $arg_ty),*) -> ($($ret_ty),*) {
        use $crate::macros::graph_resolvers::GraphResolver;
        use $crate::graph::raw::IODataTypes;
        use structural_reflection::c_tuple::{CTuple, c_tuple};

        $graph_resolver.with_resolve(stringify!($name), |graph, inputs, outputs, is_checked| {
            if is_checked {
                inputs.store_all(c_tuple!($(graph!(2 $arg: $arg_ty)),*)).expect("graph type-check error");
                graph.compute(ctx, inputs.read_only(), outputs.write_only()).expect("graph type-check error");
                outputs.load_all::<CTuple!($(graph!(3 $ret_ty)),*)>().expect("graph type-check error").into_normal()
            } else {
                unsafe {
                    inputs.store_all_unchecked(c_tuple!($(graph!(2 $arg: $arg_ty)),*));
                    graph.compute_unchecked(ctx, inputs.read_only(), outputs.write_only());
                    outputs.load_all_unchecked::<CTuple!($(graph!(3 $ret_ty)),*)>().into_normal()
                }
            }
        })
    }
},
{ 1 graph_resolver = $graph_resolver:expr;
    $vis:vis fn $name:ident($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*)
} => {
    #[allow(unsafe_op_in_unsafe_fn, unused_parens, unused_imports)]
    $vis fn $name($($arg: $arg_ty),*) -> ($($ret_ty),*) {
        use $crate::macros::graph_resolvers::GraphResolver;
        use $crate::graph::raw::IODataTypes;
        use structural_reflection::c_tuple::{CTuple, c_tuple};

        $graph_resolver.with_resolve(stringify!($name), |graph, inputs, outputs, is_checked| {
            if is_checked {
                inputs.store_all(c_tuple!($(graph!(2 $arg: $arg_ty)),*)).expect("graph type-check error");
                graph.compute(&mut (), inputs.read_only(), outputs.write_only()).expect("graph type-check error");
                outputs.load_all::<CTuple!($(graph!(3 $ret_ty)),*)>().expect("graph type-check error").into_normal()
            } else {
                unsafe {
                    inputs.store_all_unchecked(c_tuple!($(graph!(2 $arg: $arg_ty)),*));
                    graph.compute_unchecked(&mut (), inputs.read_only(), outputs.write_only());
                    outputs.load_all_unchecked::<CTuple!($(graph!(3 $ret_ty)),*)>().into_normal()
                }
            }
        })
    }
},
/* { graph_resolver = $graph_resolver:expr;
    $($vis:vis $( $ids:ident )+ ($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*));* $(;)?
} => {
    $(
        graph! { 1 graph_resolver = $graph_resolver;
            $vis $( $ids )+ ($($arg: $arg_ty),*) -> ($($ret_ty),*);
        }
    )*
}, */
{ graph_resolver = $graph_resolver:expr, ctx = $ctx_ty:ty;
    $($vis:vis $( $ids:ident )+ ($($arg:ident: $arg_ty:ty),*) -> ($($ret_ty:ty),*));* $(;)?
} => {
    $(
        graph! { 1 graph_resolver = $graph_resolver, ctx = $ctx_ty;
            $vis $( $ids )+ ($($arg: $arg_ty),*) -> ($($ret_ty),*)
        }
    )*
},
{ graph = $graph:expr $(, $($tt1:tt)*)?; $($tt2:tt)* } => {
    graph! { graph_resolver = $crate::macros::graph_resolvers::IdentityGraphResolver($graph) $(, $($tt1)*)?; $($tt2)* }
},
{ path = $path:literal, comptime_ctx = $comptime_ctx:expr $(, ctx = $ctx_ty:ty $( , $($tt1:tt)*)?)?; $($tt2:tt)*  } => {
    lazy_static::lazy_static! {
        static ref PATH_RESOLVER: ::std::sync::Mutex<$crate::macros::graph_resolvers::PathGraphResolver<'static, __first!($( $ctx_ty )?, ())>> = ::std::sync::Mutex::new($crate::macros::graph_resolvers::PathGraphResolver::new(
            $crate::macros::graph_resolvers::resolve_graph_path($path, env!("CARGO_MANIFEST_DIR"), file!()),
            &$comptime_ctx
        ));
    }

    graph! { graph_resolver = PATH_RESOLVER.lock().expect("file graph resolver poisoned") $(, ctx = $ctx_ty $(, $($tt1)*)?)?; $($tt2)* }
}
}

macro __ignore($(tt:tt)*) {}

macro __first($first:tt $(, $rest:tt)*) {
$first
}

#[cfg(test)]
mod tests {
    use std::ops::Add;
    use structural_reflection::{Qualifier, RustType};
    use structural_reflection::derive::{HasStructure, HasTypeName};
    use lazy_static::lazy_static;
    use structural_reflection::c_tuple::{c_tuple, CTuple};
    use crate::ir::{ComptimeCtx, NodeIO, NodeIOType, NodeTypeData};
    use crate::raw::{NodeTypes, NodeType, NodeTypeMetadata, NullRegion, ComputeFn};
    use crate::macros::graph;

    lazy_static! {
        static ref MY_COMPTIME_CTX: ComptimeCtx<MyRuntimeCtx> = ComptimeCtx {
            qualifier: Qualifier::local(),
            node_types: node_types()
        };
    }

    fn node_types() -> NodeTypes<MyRuntimeCtx> {
        let node_types = NodeTypes::new();
        node_types.insert("Fun", NodeType {
            compute: ComputeFn::new(|ctx, inputs, outputs| {
                let (inputs, distance_type) = inputs.load::<CTuple!(Slice<Vector3<f64>>, NonNull<DistanceType>)>()?;

                let distance = inputs.iter().flat_map(|a| inputs.map(|b| match distance_type {
                    DistanceType::Euclidean => (a - b).magnitude(),
                    DistanceType::Manhattan => (a - b).map(|x| x.abs()).sum(),
                })).min().ok_or("no inputs")?;

                outputs.store(c_tuple!(NonNull(distance)))?;
                Ok(())
            }),
            type_data: NodeTypeData {
                inputs: vec![
                    NodeIOType {
                        name: "inputs".to_string(),
                        rust_type: RustType::of_slice::<Vector3<f64>>(),
                        null_region: NullRegion::NonNull
                    },
                    NodeIOType {
                        name: "distance_type".to_string(),
                        rust_type: RustType::of::<DistanceType>(),
                        null_region: NullRegion::NonNull
                    }
                ],
                outputs: vec![
                    NodeIOType {
                        name: "distance".to_string(),
                        rust_type: RustType::of::<f64>(),
                        null_region: NullRegion::NonNull
                    }
                ]
            },
            default_inputs: vec![
                NodeIO::Hole,
                NodeIO::inline_const(DistanceType::Euclidean)
            ],
            default_default_outputs: vec![
                NodeIO::Hole
            ],
            meta: NodeTypeMetadata::default()
        });
        node_types
    }

    struct MyRuntimeCtx;
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
    #[repr(C)]
    struct Vector3<T> { x: T, y: T, z: T }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
    #[repr(C)]
    enum DistanceType { Euclidean, Manhattan }

    impl Vector3<f64> {
        fn magnitude(&self) -> f64 {
            (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
        }

        fn map(&self, f: impl Fn(f64) -> f64) -> Self {
            Self {
                x: f(self.x),
                y: f(self.y),
                z: f(self.z)
            }
        }

        fn sum(&self) -> f64 {
            self.x + self.y + self.z
        }
    }

    graph! { path = "$CARGO_MANIFEST_DIR/tests/resources/dvls/max_distance.dvl", comptime_ctx = MY_COMPTIME_CTX, ctx = MyRuntimeCtx;
        fn max_euclidean_distance(inputs: &[Vector3<f64>]) -> (f64);
        fn max_distance(inputs: &[Vector3<f64>], distance_type: DistanceType) -> (f64);
        unsafe fn max_distance_unsafe(inputs: &[Vector3<f64>], distance_type: DistanceType) -> (f64);
    }

    #[test]
    pub fn test_max_distance() {
        let vectors = [
            Vector3 { x: 1.0, y: 2.0, z: 3.0 },
            Vector3 { x: 4.0, y: 5.0, z: 6.0 },
            Vector3 { x: 7.0, y: 8.0, z: 9.0 },
        ];
        assert_eq!(
            max_euclidean_distance(&mut MyRuntimeCtx, &vectors),
            (9.0)
        );
        assert_eq!(
            max_distance(&mut MyRuntimeCtx, &vectors, DistanceType::Euclidean),
            (9.0)
        );
        assert_eq!(
            max_distance(&mut MyRuntimeCtx, &vectors, DistanceType::Manhattan),
            (24.0)
        );
        assert_eq!(
            unsafe { max_distance_unsafe(&mut MyRuntimeCtx, &vectors, DistanceType::Euclidean) },
            (9.0)
        );
    }
}