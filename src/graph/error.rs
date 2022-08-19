use std::error::Error;
use std::num::{ParseFloatError, ParseIntError, TryFromIntError};
use std::path::PathBuf;

use derive_more::{Display, Error};
use snailquote::UnescapeError;
//noinspection RsUnusedImport (intelliJ fails to detect use)
use join_lazy_fmt::Join;

use crate::graph::mutable::NodeId;
use crate::rust_type::{RustType, TypeStructBodyForm};

pub type ParseErrors = Vec<ParseError>;

#[derive(Debug, Display, Error)]
#[display(fmt = "at {}:{}, {}\n{}", line, column, "path.display()", body)]
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
    #[display(fmt = "bad escape in string: {}", _0)]
    BadEscape(#[error(source)] UnescapeError),
    #[display(fmt = "unopened '{}'", _0)]
    Unopened(#[error(not(source))] char),
    #[display(fmt = "unopened '{}'", _0)]
    Unclosed(#[error(not(source))] char),
    #[display(fmt = "unexpected ','")]
    UnexpectedComma,
}

pub type GraphFormErrors = Vec<GraphFormError>;

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
    #[display(fmt = "outputs can't have values: in node {}, output {}", node_name, output_name)]
    OutputHasValue {
        node_name: String,
        output_name: String
    },
    #[display(fmt = "node type not found: {} (in {})", type_name, node_name)]
    NodeTypeNotFound {
        type_name: String,
        node_name: String,
    },
    #[display(fmt = "node type function not found: {} (in {})", type_fn_name, node_name)]
    NodeTypeFunctionNotFound {
        type_fn_name: String,
        node_name: String,
    },
    #[display(fmt = "node type function missing ')' (in {})", node_name)]
    NodeTypeFunctionMissingRParen { #[error(not(source))] node_name: String },
    #[display(fmt = "node type function error: {} (in {})", error, node_name)]
    NodeTypeFunctionError {
        error: Box<dyn Error>,
        node_name: String
    },
    #[display(fmt = "type not found: {} (referenced from node {})", type_name, referenced_from)]
    RustTypeNotFound {
        type_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type not found: {} (referenced from struct constructor, in node {})", type_name, referenced_from)]
    RustTypeNotFoundFromStructConstructor {
        type_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type not found: {} (referenced from enum variant constructor, in node {})", type_name, referenced_from)]
    RustTypeNotFoundFromEnumVariantConstructor {
        type_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type is not a struct but value is a struct constructor: type = {} (referenced in node {})", type_name, referenced_from)]
    RustTypeNotStructFromConstructor {
        type_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "type constructor has bad form: got {} expected {} (type = {}, referenced in node {})", expected_form, actual_form, type_name, referenced_from)]
    RustTypeConstructorBadForm {
        expected_form: TypeStructBodyForm,
        actual_form: TypeStructBodyForm,
        type_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type is not an enum but value is an enum constructor: type = {} (referenced in node {})", type_name, referenced_from)]
    RustTypeNotEnumFromConstructor {
        type_name: String,
        referenced_from: NodeNameFieldName,
    },
    #[display(fmt = "variant not in type {}: {} (referenced from constructor in node {})", type_name, variant_name, referenced_from)]
    EnumVariantNotFound {
        type_name: String,
        variant_name: String,
        referenced_from: NodeNameFieldName,
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
    #[display(fmt = "this is not a node type, it's a data type: type name {}, node name {}", type_name, node_name)]
    NodeIsDataType {
        type_name: String,
        node_name: String
    },
    #[display(fmt = "type has same name as builtin type: {}, but it has an incompatible structura", name)]
    TypeConflictsWithBuiltinType { #[error(not(source))] name: String },
    #[display(fmt = "type mismatch, lengths of tuples are different: got {} expected {} (for type {}, referenced in {})", inferred_length, type_length, type_name, referenced_from)]
    TupleLengthMismatch {
        inferred_length: usize,
        type_length: usize,
        type_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "field {} not in rust type {} (referenced in {})", field_name, type_name, referenced_from)]
    RustFieldNotFound {
        field_name: String,
        type_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "multiple occurrences of field {} (referenced in {})", field_name, referenced_from)]
    RustFieldMultipleOccurrences {
        field_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch: value is {}, type is {}. Note that if the type names are the same, the contents are still different", inferred_type_name, explicit_type_name)]
    ValueTypeMismatch {
        inferred_type_name: String,
        explicit_type_name: String
    },
    #[display(fmt = "type mismatch: value is {}, type is {}. Note that if the type names are the same, the contents are still different (referenced in {})", inferred_type_name, explicit_type_name, referenced_from)]
    NestedValueTypeMismatch {
        inferred_type_name: String,
        explicit_type_name: String,
        referenced_from: NodeNameFieldName
    },
    #[display(fmt = "type mismatch: array elements are {} and {}. Note that if the type names are the same, the contents are still different", type_name_lhs, type_name_rhs)]
    ArrayElemTypeMismatch {
        type_name_lhs: String,
        type_name_rhs: String
    },
    #[display(fmt = "value has both inline and multiline definition: at {}", source)]
    InlineValueHasChildren {
        #[error(not(source))]
        source: NodeNameFieldName
    },
    #[display(fmt = "layout not resolved, we need to know the size and alignment of each item (inferred type = {}, referenced from {}", inferred_type, referenced_from)]
    ElemLayoutNotResolved {
        inferred_type: String,
        referenced_from: NodeNameFieldName
    },
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "node {} field {}", node_name, field_name)]
pub struct NodeNameFieldName {
    pub node_name: String,
    pub field_name: String
}

pub type GraphValidationErrors = Vec<GraphValidationError>;

#[derive(Debug, Display, Error)]
pub enum GraphValidationError {
    #[display(fmt = "node cycle: {}", _0)]
    Cycle(#[error(not(source))] NodeCycle)
}

pub type GraphIOCheckErrors = Vec<GraphIOCheckError>;

#[derive(Debug, Display, Error)]
pub enum GraphIOCheckError {
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
    #[display(fmt = "input type mismatch: in {}, got {} expected {}", field_name, actual, expected)]
    InputTypeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "input type can't be verified: in {}, got {} expected {}", field_name, actual, expected)]
    InputTypeMaybeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "output type mismatch: in {}, got {} expected {}", field_name, actual, expected)]
    OutputTypeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
    #[display(fmt = "output type can't be verified: in {}, got {} expected {}", field_name, actual, expected)]
    OutputTypeMaybeMismatch {
        field_name: String,
        expected: RustType,
        actual: RustType
    },
}

#[derive(Debug, Display)]
#[display(fmt = "[{}]", "\", \".join(_0)")]
pub struct NodeCycle(pub Vec<NodeId>);