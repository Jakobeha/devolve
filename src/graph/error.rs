use std::error::Error;
use std::num::{ParseFloatError, ParseIntError, TryFromIntError};
use std::path::PathBuf;

use derive_more::{Display, Error};
use snailquote::UnescapeError;
//noinspection RsUnusedImport (IntelliJ fails to detect)
use join_lazy_fmt::Join;

use crate::graph::ir::NodeId;
use structural_reflection::{RustType, RustTypeName, RustTypeNameParseErrorCause, TypeStructureBodyForm};
use crate::ir::NodeDisplay;
use crate::raw::{IOTypeCheckError, Nullability};

macro_rules! mk_errors {
    ($Errors:ident, $Error:ident, $errors:literal) => {
#[derive(Debug)]
pub struct $Errors(::std::vec::Vec<$Error>);

impl $Errors {
    pub fn new() -> Self {
        $Errors(::std::vec::Vec::new())
    }

    pub fn push(&mut self, error: $Error) {
        self.0.push(error)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl ::std::fmt::Display for $Errors {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        write!(f, "{} errors:", $errors)?;
        if self.0.is_empty() {
            write!(f, " none")?;
        } else {
            for error in &self.0 {
                write!(f, "\n{}", error)?;
            }
        }
        Ok(())
    }
}

impl Error for $Errors {}

impl ::std::iter::FromIterator<$Error> for $Errors {
    fn from_iter<T: ::std::iter::IntoIterator<Item = $Error>>(iter: T) -> Self {
        $Errors(iter.into_iter().collect())
    }
}

impl ::std::iter::IntoIterator for $Errors {
    type Item = $Error;
    type IntoIter = ::std::vec::IntoIter<$Error>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
    }
}

mk_errors!(ParseErrors, ParseError, "parse");
mk_errors!(GraphFormErrors, GraphFormError, "IR");
mk_errors!(GraphValidationErrors, GraphValidationError, "validation");
mk_errors!(GraphIOCheckErrors, GraphIOCheckError, "input/output check");

#[derive(Debug, Display, Error)]
#[display(fmt = "- {}:{}:{}\n    {}", "path.display()", "line + 1", "column + 1", body)]
pub struct ParseError {
    pub path: PathBuf,
    pub line: usize,
    pub column: usize,
    pub body: ParseErrorBody
}


#[derive(Debug, Display, Error)]
pub enum ParseErrorBody {
    #[display(fmt = "io error: {}", _0)]
    IoError(#[error(source)] std::io::Error),
    #[display(fmt = "bad indentation")]
    BadIndentation,
    #[display(fmt = "expected more ({})", _0)]
    ExpectedMore(#[error(not(source))] &'static str),
    #[display(fmt = "expected less")]
    ExpectedLess,
    #[display(fmt = "expected {}", _0)]
    Expected(#[error(not(source))] &'static str),
    #[display(fmt = "parsing type, {}", cause)]
    ParsingType {
        #[error(not(source))]
        cause: RustTypeNameParseErrorCause
    },
    #[display(fmt = "duplicate type: {}", name)]
    DuplicateType { #[error(not(source))] name: String },
    #[display(fmt = "duplicate node: {}" name)]
    DuplicateNode { #[error(not(source))] name: String },
    #[display(fmt = "divider '===' not allowed here")]
    UnexpectedDivider,
    #[display(fmt = "mixed fields and tuple items")]
    MixedFieldsAndTupleItems,
    #[display(fmt = "couldn't parse integer: {}", _0)]
    BadInteger(#[error(source)] ParseIntError),
    #[display(fmt = "couldn't parse float: {}", _0)]
    BadFloat(#[error(source)] ParseFloatError),
    #[display(fmt = "negative array length")]
    BadArrayLength(#[error(source)] TryFromIntError),
    #[display(fmt = "integer is too large")]
    BadIntSize(#[error(source)] TryFromIntError),
    #[display(fmt = "bad escape in string: {}", _0)]
    BadEscape(#[error(source)] UnescapeError),
    #[display(fmt = "bad header: {}", _0)]
    BadHeader(#[error(not(source))] String),
    #[display(fmt = "unopened '{}'", _0)]
    Unopened(#[error(not(source))] char),
    #[display(fmt = "unopened '{}'", _0)]
    Unclosed(#[error(not(source))] char),
    #[display(fmt = "unexpected ','")]
    UnexpectedComma,
}

#[derive(Debug, Display, Error)]
pub enum GraphFormError {
    #[display(fmt = "no 'Input' node")]
    NoInput,
    #[display(fmt = "no 'Output' node")]
    NoOutput,
    #[display(fmt = "input can't be a view or computation")]
    InputHasCompute,
    #[display(fmt = "output can't be a view or computation")]
    OutputHasCompute,
    #[display(fmt = "'Input' node has inputs")]
    InputHasInputs,
    #[display(fmt = "'Output' node has outputs")]
    OutputHasOutputs,
    // TODO part refs
    #[display(fmt = "input refs which are parts are not yet implemented: in node {}, output {}", node_name, field_name)]
    TodoPartRef {
        node_name: String,
        field_name: String
    },
    #[display(fmt = "node type not found: {} (in {})", type_name, node_name)]
    NodeTypeNotFound {
        type_name: String,
        node_name: String,
    },
    #[display(fmt = "node type function missing ')' (in {})", node_name)]
    NodeTypeFunctionMissingRParen { #[error(not(source))] node_name: String },
    #[display(fmt = "node type function error: {} (in {})", error, node_name)]
    NodeTypeFunctionError {
        error: Box<dyn Error>,
        node_name: String
    },
    #[display(fmt = "node not found: {} (referenced from node {})", node_name, referenced_from)]
    NodeNotFound {
        node_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "field {} not in node {} (referenced from {})", field_name, node_name, referenced_from)]
    NodeFieldNotFound {
        field_name: String,
        node_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "can't refer to {} from node {} because it forms a cycle", node_name, referenced_from)]
    CyclicReference {
        node_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "node has multiple position metadatas: {}", node_name)]
    NodeMetaMultiplePos {
        node_name: String
    },
    #[display(fmt = "node has multiple color metadatas: {}", node_name)]
    NodeMetaMultipleColor {
        node_name: String
    },
    #[display(fmt = "this is not a node type, it's a data type: type name {}, node name {}", type_name, node_name)]
    NodeIsDataType {
        type_name: String,
        node_name: String
    },
    #[display(fmt = "default values in types aren't supported, default values are in only in nodes (field {} in type {})", field_name, type_def_name)]
    FieldDefaultValueNotSupported {
        type_def_name: String,
        field_name: String
    },
    #[display(fmt = "type def layout not resolved, we need to know the size and alignment of each item: {}", type_def_name)]
    TypeDefLayoutNotResolved {
        type_def_name: String
    },
    #[display(fmt = "type has same name as a registered native-Rust type, but an incompatible structure: {}", "type_name.qualified()")]
    TypeConflictsWithRegisteredType {
        #[error(not(source))]
        type_name: RustTypeName
    },
    #[display(fmt = "type not found: {} (referenced from node {})", "type_name.unqualified()", referenced_from)]
    RustTypeNotFound {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type not found: {} (referenced from struct constructor, in node {})", "type_name.unqualified()", referenced_from)]
    RustTypeNotFoundFromStructConstructor {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type not found: {} (referenced from enum variant constructor, in node {})", "type_name.unqualified()", referenced_from)]
    RustTypeNotFoundFromEnumVariantConstructor {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type is not a struct but value is a struct constructor: type = {} (referenced in node {})", "type_name.unqualified()", referenced_from)]
    RustTypeNotStructFromConstructor {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type constructor has bad form: got {} expected {} (type = {}, referenced in node {})", expected_form, actual_form, "type_name.unqualified()", referenced_from)]
    RustTypeConstructorBadForm {
        expected_form: TypeStructureBodyForm,
        actual_form: TypeStructureBodyForm,
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type is not an enum but value is an enum constructor: type = {} (referenced in node {})", "type_name.unqualified()", referenced_from)]
    RustTypeNotEnumFromConstructor {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "variant not in type {}: {} (referenced from constructor in node {})", "type_name.unqualified()", variant_name, referenced_from)]
    EnumVariantNotFound {
        type_name: RustTypeName,
        variant_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type mismatch, lengths of arrays are different: got {} expected {} (for type {}, referenced in {})", actual_length, type_length, "type_name.unqualified()", referenced_from)]
    ArrayLengthMismatch {
        actual_length: usize,
        type_length: usize,
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch, lengths of tuples are different: got {} expected {} (for type {}, referenced in {})", actual_length, type_length, "type_name.unqualified()", referenced_from)]
    TupleLengthMismatch {
        actual_length: usize,
        type_length: usize,
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch, lengths of tuples or structs are different: got {} expected {} (for type {}, referenced in {})", actual_length, type_length, "type_name.unqualified()", referenced_from)]
    TupleOrTupleStructLengthMismatch {
        actual_length: usize,
        type_length: usize,
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "field {} not in rust type {} (referenced in {})", field_name, "type_name.unqualified()", referenced_from)]
    RustFieldNotFound {
        field_name: String,
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "multiple occurrences of field {} (referenced in {})", field_name, referenced_from)]
    RustFieldMultipleOccurrences {
        field_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "not a tuple (type = {}, referenced in {})", "type_name.unqualified()", referenced_from)]
    NotATuple {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "not a tuple or tuple struct (type = {}, referenced in {})", "type_name.unqualified()", referenced_from)]
    NotATupleOrTupleStruct {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "not a field struct (type = {}, referenced in {})", "type_name.unqualified()", referenced_from)]
    NotAFieldStruct {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "not an array (type = {}, referenced in {})", "type_name.unqualified()", referenced_from)]
    NotAnArray {
        type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch: actual type is {}, but node type's field type is {} (referenced from {})\nNote that if the type names are the same, the contents are still different", "self_type_name.unqualified()", "inherited_type_name.unqualified()", referenced_from)]
    InheritedNodeTypeMismatch {
        self_type_name: RustTypeName,
        inherited_type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch: value is {}, type is {}\nNote that if the type names are the same, the contents are still different", "inferred_type_name.unqualified()", "explicit_type_name.unqualified()")]
    ValueTypeMismatch {
        inferred_type_name: RustTypeName,
        explicit_type_name: RustTypeName
    },
    #[display(fmt = "type mismatch: value is {}, type is {} (referenced in {})\nNote that if the type names are the same, the contents are still different", "inferred_type_name.unqualified()", "explicit_type_name.unqualified()", referenced_from)]
    NestedValueTypeMismatch {
        inferred_type_name: RustTypeName,
        explicit_type_name: RustTypeName,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch: array elements are {} and {}. Note that if the type names are the same, the contents are still different", "type_name_lhs.unqualified()", "type_name_rhs.unqualified()")]
    ArrayElemTypeMismatch {
        type_name_lhs: RustTypeName,
        type_name_rhs: RustTypeName
    },
    #[display(fmt = "value has both inline and multiline definition: at {}", source)]
    InlineValueHasChildren {
        #[error(not(source))]
        source: NodeNameFieldName
    },
    #[display(fmt = "pointer to unregistered type: {}", "refd_type_name.unqualified()")]
    PointerToUnregisteredType {
        #[error(not(source))]
        refd_type_name: RustTypeName
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "node {} field {}", node_name, field_name)]
pub struct NodeNameFieldName {
    pub node_name: String,
    pub field_name: String
}

#[derive(Debug, Display, Error)]
pub enum GraphValidationError {
    #[display(fmt = "node cycle: {}", _0)]
    Cycle(#[error(not(source))] NodeCycle),
    #[display(fmt = "node has no compute function: {}", node)]
    NoCompute {
        node: NodeDisplay
    },
    #[display(fmt = "I/O nullability mismatch: {} -> {} (in {})", output_nullability, input_nullability, referenced_from)]
    IONullabilityMismatch {
        output_nullability: Nullability,
        input_nullability: Nullability,
        referenced_from: NodeDisplayInputName
    },
    #[display(fmt = "I/O type mismatch: {} -> {} (in {})", "output_type.type_name.unqualified()", "input_type.type_name.unqualified()", referenced_from)]
    IOTypeMismatch {
        output_type: RustType,
        input_type: RustType,
        referenced_from: NodeDisplayInputName
    },
    #[display(fmt = "I/O type maybe mismatch: {} -> {} (in {})", "output_type.type_name.unqualified()", "input_type.type_name.unqualified()", referenced_from)]
    IOTypeMaybeMismatch {
        output_type: RustType,
        input_type: RustType,
        referenced_from: NodeDisplayInputName
    },
    #[display(fmt = "unknown type (referenced from {})", referenced_from)]
    UnknownType {
        referenced_from: NodeDisplayInputName
    },
    #[display(fmt = "type has unknown size: {} (referenced from {})", "type_.type_name.unqualified()", referenced_from)]
    UnknownSizedType {
        type_: RustType,
        referenced_from: NodeDisplayInputName
    },
    #[display(fmt = "type has unknown alignment: {} (referenced from {})", "type_.type_name.unqualified()", referenced_from)]
    UnknownAlignedType {
        type_: RustType,
        referenced_from: NodeDisplayInputName
    },
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "node {} input {}", node, input_name)]
pub struct NodeDisplayInputName {
    pub node: NodeDisplay,
    pub input_name: String
}

#[derive(Debug, Display, Error)]
pub enum GraphIOCheckError {
    IOTypeCheckError(IOTypeCheckError),
    #[display(fmt = "inputs count mismatch: got {} expected {}", actual, expected)]
    InputsCountMismatch {
        actual: usize,
        expected: usize
    },
    #[display(fmt = "outputs count mismatch: got {} expected {}", actual, expected)]
    OutputsCountMismatch {
        actual: usize,
        expected: usize
    },
    #[display(fmt = "input type mismatch: in {}, {} not a subtype of {}", field_name, "actual.type_name.unqualified()", "expected.type_name.unqualified()")]
    InputTypeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "input type can't be verified: in {}, {} maybe not a subtype of {}", field_name, "actual.type_name.unqualified()", "expected.type_name.unqualified()")]
    InputTypeMaybeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "input nullability mismatch in {}, {} not a subset of {}", field_name, actual, expected)]
    InputNullabilityMismatch {
        field_name: String,
        expected: Nullability,
        actual: Nullability
    },
    #[display(fmt = "output type mismatch: in {}, {} not a supertype of {}", field_name, "actual.type_name.unqualified()", "expected.type_name.unqualified()")]
    OutputTypeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "output type can't be verified: in {}, {} maybe not a supertype of {}", field_name, "actual.type_name.unqualified()", "expected.type_name.unqualified()")]
    OutputTypeMaybeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "output nullability mismatch in {}, {} not a superset of {}", field_name, actual, expected)]
    OutputNullabilityMismatch {
        field_name: String,
        expected: Nullability,
        actual: Nullability
    },
}

#[derive(Debug, Display)]
#[display(fmt = "[{}]", "\", \".join(_0)")]
pub struct NodeCycle(pub Vec<NodeId>);

impl From<IOTypeCheckError> for GraphIOCheckError {
    fn from(error: IOTypeCheckError) -> Self {
        GraphIOCheckError::IOTypeCheckError(error)
    }
}