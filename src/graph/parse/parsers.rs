use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use logos::{Lexer, Logos, Span, SpannedIter};
use derive_more::Display;
use snailquote::unescape;
use crate::graph::error::{ParseErrors, ParseError, ParseErrorBody};
use crate::misc::extract::extract;
use crate::graph::parse::lexer_ext::LexerExt;
use crate::graph::parse::types::{SerialBody, SerialEnumType, SerialEnumVariantType, SerialField, SerialFieldElem, SerialFieldType, SerialGraph, SerialNode, SerialRustType, SerialStructType, SerialTupleItem, SerialType, SerialTypeBody, SerialValueHead};

#[derive(Logos)]
enum GraphToken {
    #[token("include")]
    Include,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,

    #[regex("[~!@#$%^&*-=+|:;,.?/(\\[{<>}\\])]", |lex| lex.slice().chars().next().unwrap())]
    Punct(char),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    #[regex("`[^`]*`")]
    Ident,
    #[regex("-?[0-9]+", priority = 2, callback = |lex| lex.slice().parse::<i64>())]
    Integer(Option<i64>),
    #[regex("-?[0-9]+\\.[0-9]*", |lex| lex.slice().parse::<f64>())]
    #[regex("-?[0-9]+\\.[0-9]+e[0-9]+", |lex| lex.slice().parse::<f64>())]
    #[regex("-?[0-9]+e[0-9]+", |lex| lex.slice().parse::<f64>())]
    Float(Option<f64>),
    #[regex("\"([^\"]|\\\\\")*\"")]
    String,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}


struct GraphParser<'a> {
    graph: &'a mut SerialGraph,
    errors: &'a mut Vec<ParseError>,
    path: PathBuf,
    token_buffer: &'a mut Vec<(GraphToken, Span)>,
}

enum BlockParser<'a, 'b: 'a> {
    Include {
        p: &'a mut GraphParser<'b>,
        path: PathBuf
    },
    StructType {
        p: &'a mut GraphParser<'b>,
        name: String,
        struct_type: SerialStructType,
    },
    EnumType {
        p: &'a mut GraphParser<'b>,
        name: String,
        enum_type: SerialEnumType,
    },
    Node {
        p: &'a mut GraphParser<'b>,
        name: String,
        node: SerialNode
    },
    Null
}

struct AbstractTreeParser<'a, 'b: 'a, Item> {
    p: &'a mut GraphParser<'b>,
    items: Vec<(usize, Item)>,
    base_indent: usize,
    current_item: Option<(usize, Item)>
}

struct FieldElemParser<'a, 'b: 'a> {
    p: AbstractTreeParser<'a, 'b, FieldElemParserItem>,
    input_fields: &'a mut Vec<SerialFieldElem>,
    output_fields: &'a mut Vec<SerialFieldElem>,
    field_side: FieldSide
}

enum FieldElemParserItem {
    Divider,
    Header { header: String },
    Field(SerialField)
}

struct BodyParser<'a, 'b: 'a> {
    p: AbstractTreeParser<'a, 'b, BodyParserItem>,
    body: &'a mut SerialBody
}

enum BodyParserItem {
    TupleItem(SerialTupleItem),
    Field(SerialField),
}

struct EnumVariantTypeParser<'a, 'b: 'a> {
    p: AbstractTreeParser<'a, 'b, SerialEnumVariantType>,
    variants: &'a mut Vec<SerialEnumVariantType>
}

struct TypeBodyParser<'a, 'b: 'a> {
    p: AbstractTreeParser<'a, 'b, TypeBodyParserItem>,
    body: &'a mut SerialTypeBody
}

enum TypeBodyParserItem {
    TupleItem(SerialRustType),
    Field(SerialFieldType)
}

enum FieldSide {
    Input,
    Output
}

impl SerialGraph {
    pub fn parse_from(path: &Path) -> Result<Self, ParseErrors> {
        let mut graph = SerialGraph::new();
        let mut errors = Vec::new();
        let mut token_buffer = Vec::new();

        {
            GraphParser {
                graph: &mut graph,
                errors: &mut errors,
                path: path.to_path_buf(),
                token_buffer: &mut token_buffer
            }.parse();
        }

        if errors.is_empty() {
            Ok(graph)
        } else {
            Err(errors)
        }
    }

    fn new() -> Self {
        SerialGraph {
            types: HashMap::new(),
            nodes: HashMap::new()
        }
    }
}

impl<'a> GraphParser<'a> {
    fn parse(&mut self) {
        let input = match Self::get_file(&self.path) {
            Ok(file) => file,
            Err(error) => {
                self.errors.push(ParseError {
                    path: self.path.to_path_buf(),
                    line: 0,
                    column: 0,
                    body: ParseErrorBody::IoError(error)
                });
                return;
            }
        };

        let lines = input.lines();
        let mut block = Vec::new();

        let finish_block = |block: &mut Vec<(usize, String)>| {
            if !block.is_empty() {
                self.parse_block(block);
                block.clear();
                sub_blocks.clear();
            }
        };

        for (line_num, line) in lines.enumerate() {
            match line {
                Err(error) => {
                    errors.push(ParseError {
                        path: self.path.to_path_buf(),
                        line: line_num,
                        column: 0,
                        body: ParseErrorBody::IoError(error)
                    });
                    return;
                }
                Ok(line) => if line.is_empty() || line.chars().all(|char| char.is_whitespace()) {
                    finish_block(&mut block);
                } else {
                    block.push((line_num, line));
                }
            }
        }
        finish_block(&mut block);
    }

    fn get_file(path: &Path) -> Result<BufReader<File>, std::io::Error> {
        let file = File::options().read(true).open(path)?;
        Ok(BufReader::new(file))
    }

    fn parse_block(&mut self, lines: &[(usize, String)]) {
        let (first_line_num, first_line) = lines.first().expect("expected at least 1 line in block or it should've been skipped");
        let lines = &lines[1..];

        match self.parse_head(first_line) {
            Ok(mut block_parser) => {
                block_parser.parse_rest(lines);
                block_parser.finish(*first_line_num);
            },
            Err((column, body)) => {
                self.errors.push(ParseError {
                    path: self.path.to_path_buf(),
                    line: *first_line_num,
                    column,
                    body
                })
            }
        }
    }

    fn parse_head<'z>(&'z mut self, first_line: &str) -> Result<BlockParser<'z, 'a>, (usize, ParseErrorBody)> {
        let mut lexer = Lexer::<GraphToken>::new(first_line);

        Ok(match lexer.next().expect("expected at least 1 token in line or it should've been skipped") {
            GraphToken::Include => {
                lexer.munch("string", |token| extract!(token, GraphToken::String))?;
                let str = match unescape(lexer.slice()) {
                    Ok(str) => str,
                    Err(error) => Err((lexer.span().start, ParseErrorBody::BadEscape(error)))?
                };
                lexer.munch_end()?;

                let mut path = self.path.to_path_buf();
                path.push(str);

                BlockParser::Include {
                    p: self,
                    path
                }
            },
            GraphToken::Struct => {
                lexer.munch("ident", |token| extract!(token, GraphToken::Ident))?;
                let name = lexer.slice().to_string();
                lexer.munch_end()?;

                BlockParser::StructType {
                    p: self,
                    name,
                    struct_type: SerialStructType {
                        body: SerialTypeBody::None
                    }
                }
            },
            GraphToken::Enum => {
                lexer.munch("ident", |token| extract!(token, GraphToken::Ident))?;
                let name = lexer.slice().to_string();
                lexer.munch_end()?;

                BlockParser::EnumType {
                    p: self,
                    name,
                    enum_type: SerialEnumType {
                        variants: Vec::new()
                    }
                }
            }
            GraphToken::Ident => {
                let name = lexer.slice().to_string();
                let type_ = match lexer.next() {
                    None => None,
                    Some((GraphToken::Colon, _)) => {
                        let rust_type = munch_type(&mut lexer)?;
                        lexer.munch_end()?;
                        Some(rust_type);
                    }
                };

                BlockParser::Node {
                    p: self,
                    name,
                    node: SerialNode {
                        node_type: type_,
                        input_fields: vec![],
                        output_fields: vec![]
                    }
                }
            },
            _ => Err((span.start, ParseErrorBody::Expected("include, struct, enum, or ident")))?
        })
    }
}

impl<'a, 'b: 'a> BlockParser<'a, 'b> {
    fn parse_rest(&mut self, lines: &[(usize, String)]) {
        match self {
            BlockParser::Include { p, path: _ } => {
                if !lines.is_empty() {
                    p.errors.push(ParseError {
                        path: p.path.to_path_buf(),
                        line: lines.first().unwrap().0,
                        column: 0,
                        body: ParseErrorBody::ExpectedLess
                    });
                }
            }
            BlockParser::StructType { p, name: _, struct_type } => {
                TypeBodyParser::parse(p, lines, &mut struct_type.body);
            }
            BlockParser::EnumType { p, name: _, enum_type } => {
                EnumVariantTypeParser::parse(p, lines, &mut enum_type.variants);
            }
            BlockParser::Node { p, name: _, node } => {
                FieldElemParser::parse(p, lines, &mut node.input_fields, &mut node.output_fields);
            }
            BlockParser::Null => {}
        }
    }

    fn finish(self, start_line_num: usize) {
        match self {
            BlockParser::Include { p, path } => {
                p.path = path;
                p.parse()
            },
            BlockParser::StructType { p, name, struct_type } => {
                if p.graph.types.contains_key(&name) {
                    p.errors.push(ParseError {
                        path: p.path.to_path_buf(),
                        line: start_line_num,
                        column: 0,
                        body: ParseErrorBody::DuplicateType { name }
                    });
                } else {
                    p.graph.types.insert(name, SerialType::Struct(struct_type));
                }
            }
            BlockParser::EnumType { p, name, enum_type } => {
                if p.graph.types.contains_key(&name) {
                    p.errors.push(ParseError {
                        path: p.path.to_path_buf(),
                        line: start_line_num,
                        column: 0,
                        body: ParseErrorBody::DuplicateType { name }
                    })
                } else {
                    p.graph.types.insert(name, SerialType::Enum(enum_type))
                }
            }
            BlockParser::Node { p, name, node,  } => {
                if p.graph.nodes.contains_key(&name) {
                    p.errors.push(ParseError {
                        path: p.path.to_path_buf(),
                        line: start_line_num,
                        column: 0,
                        body: ParseErrorBody::DuplicateNode { name }
                    })
                } else {
                    p.graph.nodes.insert(name, node)
                }
            }
            BlockParser::Null => {}
        }
    }
}

impl<'a, 'b, Item> AbstractTreeParser<'a, 'b, Item> {
    fn new(p: &'a mut GraphParser<'b>) -> Self {
        AbstractTreeParser {
            p,
            items: Vec::new(),
            base_indent: 0,
            current_item: None
        }
    }

    fn parse(
        &mut self,
        lines: &[(usize, String)],
        parse_item: impl FnMut(&str) -> Result<Item, (usize, ParseErrorBody)>,
        parse_item_children: impl FnMut(&mut GraphParser<'b>, &mut Item, &[(usize, String)])
    ) {
        for (line_index, (line_num, line)) in lines.iter().enumerate() {
            let indent = line.chars().filter(|c| c.is_whitespace()).count();
            if self.base_indent == 0 {
                self.base_indent = indent;
            }

            if indent < self.base_indent {
                self.p.errors.push(ParseError {
                    path: self.p.path.to_path_buf(),
                    line: *line_num,
                    column: indent,
                    body: ParseErrorBody::BadIndentation
                });
                self.base_indent = indent;
            }

            if indent > self.base_indent {
                let end_index = line_index + lines.iter()
                    .skip(line_index)
                    .take_while(|line| {
                        let indent = line.chars().filter(|c| c.is_whitespace()).count();
                        indent != base_indent
                    }).count();
                let inner_lines = &lines[line_index..=end_index];

                let outer_indent = indent;
                for (line_num, line) in inner_lines {
                    let indent = line.chars().filter(|c| c.is_whitespace()).count();
                    if indent < outer_indent {
                        self.p.errors.push(ParseError {
                            path: self.p.path.to_path_buf(),
                            line: *line_num,
                            column: indent,
                            body: ParseErrorBody::BadIndentation
                        });
                    }
                }

                // Will be Some unless we had an error parsing a field
                if let Some((_, mut current_item)) = self.current_item.as_mut() {
                    parse_item_children(&mut self.p, &mut current_item, inner_lines);
                }
            } else {
                self.finish_item();
                match parse_item(line.trim()) {
                    Ok(item) => self.current_item = Some((*line_num, item)),
                    Err((column, body)) => {
                        self.p.errors.push(ParseError {
                            path: self.g.path.to_path_buf(),
                            line: *line_num,
                            column,
                            body
                        })
                    }
                }
            }
        }
    }

    fn finish(mut self) -> (&'a mut GraphParser<'b>, Vec<(usize, Item)>) {
        self.finish_item();
        (self.p, self.items)
    }

    fn finish_item(&mut self) {
        if let Some(current_item) = self.current_item.take() {
            self.items.push(current_item);
        }
    }
}

impl<'a, 'b> FieldElemParser<'a, 'b> {
    fn parse(
        p: &'a mut GraphParser<'b>,
        lines: &[(usize, String)],
        input_fields: &'a mut Vec<SerialFieldElem>,
        output_fields: &'a mut Vec<SerialFieldElem>
    ) {
        let mut parser = FieldElemParser::new(p, input_fields, output_fields);
        parser._parse(lines);
        parser.finish();
    }

    fn new(
        p: &'a mut GraphParser<'b>,
        input_fields: &'a mut Vec<SerialFieldElem>,
        output_fields: &'a mut Vec<SerialFieldElem>
    ) -> Self {
        FieldElemParser {
            p: AbstractTreeParser::new(p),
            input_fields,
            output_fields,
            field_side: FieldSide::Input
        }
    }

    fn _parse(&mut self, lines: &[(usize, String)]) {
        self.p.parse(
            lines,
            parse_divider_or_header_or_field,
            |p, item, lines| {
                match item {
                    FieldElemParserItem::Divider | FieldElemParserItem::Header { header: _ } => {
                        expect_no_lines(p, lines)
                    }
                    FieldElemParserItem::Field(field) => {
                        BodyParser::parse(p, lines, &mut field.value_children);
                    }
                }
            }
        )
    }

    fn finish(mut self) {
        let (p, items) = self.p.finish();
        for (item_line_num, item) in items.into_iter() {
            match item {
                FieldElemParserItem::Divider => match self.field_side {
                    FieldSide::Input => self.field_side = FieldSide::Output,
                    _ => {
                        p.errors.push(ParseError {
                            path: p.path.as_path_buf(),
                            line: item_line_num,
                            column: 0,
                            body: ParseErrorBody::UnexpectedDivider
                        })
                    }
                },
                FieldElemParserItem::Header { header } => {
                    let fields = match self.field_side {
                        FieldSide::Input => &mut self.input_fields,
                        FieldSide::Output => &mut self.output_fields,
                    };
                    fields.push(SerialFieldElem::Header { header })
                },
                FieldElemParserItem::Field(field) => {
                    let fields = match self.field_side {
                        FieldSide::Input => &mut self.input_fields,
                        FieldSide::Output => &mut self.output_fields
                    };
                    fields.push(SerialFieldElem::Field(field))
                }
            }
        }
    }
}

impl<'a, 'b> BodyParser<'a, 'b> {
    fn parse(
        p: &'a mut GraphParser<'b>,
        lines: &[(usize, String)],
        body: &'a mut SerialBody
    ) {
        let mut parser = BodyParser::new(p, body);
        parser._parse(lines);
        parser.finish();
    }

    fn new(
        p: &'a mut GraphParser<'b>,
        body: &'a mut SerialBody
    ) -> Self {
        BodyParser {
            p: AbstractTreeParser::new(p),
            body
        }
    }

    fn _parse(&mut self, lines: &[(usize, String)]) {
        self.p.parse(
            lines,
            parse_field_or_tuple_item,
            |p, item, lines| {
                match item {
                    BodyParserItem::TupleItem(tuple_item) => {
                        BodyParser::parse(p, lines, &mut tuple_item.value_children);
                    }
                    FieldElemParserItem::Field(field) => {
                        BodyParser::parse(p, lines, &mut field.value_children);
                    }
                }
            }
        )
    }

    fn finish(mut self) {
        let (p, items) = self.p.finish();
        debug_assert!(matches!(self.body, SerialBody::None), "finish with existing SerialBody unsupported");
        if !items.is_empty() {
            if items.iter().all(|(_, item)| matches!(item, BodyParserItem::Field(_))) {
                let items = items.into_iter().map(|(_, item)| extract!(item, BodyParserItem::Field(field)).unwrap()).collect::<Vec<_>>();
                *self.body = SerialBody::Fields(items);
            } else if items.iter().all(|(_, item)| matches!(item, BodyParserItem::TupleItem(_))) {
                let items = items.into_iter().map(|(_, item)| extract!(item, BodyParserItem::TupleItem(tuple_item)).unwrap()).collect::<Vec<_>>();
                *self.body = SerialBody::Tuple(items);
            } else {
                p.errors.push(ParseError {
                    path: p.path.to_path_buf(),
                    line: *items.first().unwrap().0,
                    column: 0,
                    body: ParseErrorBody::MixedFieldsAndTupleItems
                })
            }
        }
    }
}

impl<'a, 'b> EnumVariantTypeParser<'a, 'b> {
    fn parse(
        p: &'a mut GraphParser<'b>,
        lines: &[(usize, String)],
        variants: &mut Vec<SerialEnumVariantType>
    ) {
        let mut parser = EnumVariantTypeParser::new(p, variants);
        parser._parse(lines);
        parser.finish();
    }

    fn new(
        p: &'a mut GraphParser<'b>,
        variants: &mut Vec<SerialEnumVariantType>
    ) -> Self {
        EnumVariantTypeParser {
            p: AbstractTreeParser::new(p),
            variants
        }
    }

    fn _parse(&mut self, lines: &[(usize, String)]) {
        self.p.parse(
            lines,
            parse_ident,
            |p, item, lines| {
                TypeBodyParser::parse(p, lines, &mut item.body)
            }
        )
    }

    fn finish(mut self) {
        let (_, items) = self.p.finish();
        for (_, item) in items.into_iter() {
            self.variants.push(item);
        }
    }
}

impl<'a, 'b> TypeBodyParser<'a, 'b> {
    fn parse(
        p: &'a mut GraphParser<'b>,
        lines: &[(usize, String)],
        body: &'a mut SerialTypeBody
    ) {
        let mut parser = TypeBodyParser::new(p, body);
        parser._parse(lines);
        parser.finish();
    }

    fn new(
        p: &'a mut GraphParser<'b>,
        body: &'a mut SerialTypeBody
    ) -> Self {
        TypeBodyParser {
            p: AbstractTreeParser::new(p),
            body
        }
    }

    fn _parse(&mut self, lines: &[(usize, String)]) {
        self.p.parse(
            lines,
            parse_field_type_or_tuple_item_type,
            |p, item, lines| {
                match item {
                    TypeBodyParserItem::TupleItem(_) => expect_no_lines(p, lines),
                    TypeBodyParserItem::Field(field) => {
                        BodyParser::parse(p, lines, &mut field.default_value_children)
                    }
                }
            }
        )
    }

    fn finish(mut self) {
        let (p, items) = self.p.finish();
        debug_assert!(matches!(self.body, SerialTypeBody::None), "finish with existing SerialTypeBody unsupported");
        if !items.is_empty() {
            if items.iter().all(|(_, item)| matches!(item, TypeBodyParserItem::Field(_))) {
                let items = items.into_iter().map(|(_, item)| extract!(item, TypeBodyParserItem::Field(field)).unwrap()).collect::<Vec<_>>();
                *self.body = SerialTypeBody::Fields(items);
            } else if items.iter().all(|(_, item)| matches!(item, TypeBodyParserItem::TupleItem(_))) {
                let items = items.into_iter().map(|(_, item)| extract!(item, TypeBodyParserItem::TupleItem(tuple_item)).unwrap()).collect::<Vec<_>>();
                *self.body = SerialTypeBody::Tuple(items);
            } else {
                p.errors.push(ParseError {
                    path: p.path.to_path_buf(),
                    line: *items.first().unwrap().0,
                    column: 0,
                    body: ParseErrorBody::MixedFieldsAndTupleItems
                })
            }
        }
    }
}

fn parse_divider_or_header_or_field(line: &str) -> Result<FieldElemParserItem, (usize, ParseErrorBody)> {
    if line == "===" {
        Ok(FieldElemParserItem::Divider)
    } else if line.starts_with("--") {
        Ok(FieldElemParserItem::Header { header: line.to_string() })
    } else {
        parse_field(line)
    }
}

fn parse_field_or_tuple_item(line: &str) -> Result<BodyParserItem, (usize, ParseErrorBody)> {
    // fields are always lowercase
    let first_char = line.chars().first().unwrap_or(' ');
    if first_char.is_lowercase() || first_char == '_' {
        parse_field(line).map(BodyParserItem::Field)
    } else {
        parse_tuple_item(line).map(BodyParserItem::TupleItem)
    }
}

fn parse_field_type_or_tuple_item_type(line: &str) -> Result<TypeBodyParserItem, (usize, ParseErrorBody)> {
    // fields are always lowercase
    let first_char = line.chars().first().unwrap_or(' ');
    if first_char.is_lowercase() || first_char == '_' {
        parse_field_type(line).map(TypeBodyParserItem::Field)
    } else {
        parse_rust_type(line).map(TypeBodyParserItem::TupleItem)
    }
}

fn parse_field(line: &str) -> Result<SerialField, (usize, ParseErrorBody)> {
    parse_abstract_field(line, |name, rust_type, value| SerialField {
        name,
        rust_type,
        value,
        value_children: SerialBody::None
    })
}

fn parse_field_type(line: &str) -> Result<SerialFieldType, (usize, ParseErrorBody)> {
    parse_abstract_field(line, |name, rust_type, value| SerialFieldType {
        name,
        rust_type,
        default_value: value,
        default_value_children: SerialBody::None
    })
}

fn parse_abstract_field<Field>(line: &str, mk_field: fn(String, Option<SerialRustType>, Option<SerialValueHead>) -> Field) -> Result<Field, (usize, ParseErrorBody)> {
    let mut lexer = Lexer::<GraphToken>::new(line);
    lexer.munch("ident", |token| extract!(token, GraphToken::Ident))?;
    let name = lexer.slice().to_string();
    let (rust_type, value) = match lexer.next() {
        None => (None, None),
        Some(GraphToken::Punct(':')) => {
            let rust_type = munch_type(&mut lexer)?;
            match lexer.next() {
                None => (Some(rust_type), None),
                Some(GraphToken::Punct('=')) => {
                    let value = munch_value(&mut lexer)?;
                    lexer.munch_end()?;
                    (Some(rust_type), Some(value))
                },
                Some(_) => return Err((lexer.column(), ParseErrorBody::Expected("= or end of line")))
            }
        },
        Some(GraphToken::Punct('=')) => {
            let value = munch_value(&mut lexer)?;
            lexer.munch_end()?;
            (None, Some(value))
        },
        Some(_) => return Err((lexer.column(), ParseErrorBody::Expected(":, =, or end of line")))
    };

    Ok(mk_field(name, rust_type, value))
}

fn parse_tuple_item(line: &str) -> Result<SerialTupleItem, (usize, ParseErrorBody)> {
    let mut lexer = Lexer::<GraphToken>::new(line);
    let value = munch_value_or_underscore(&mut lexer)?;
    // Rust type may not be allowed in the future
    let rust_type = match lexer.next() {
        None => None,
        Some(GraphToken::Punct(':')) => {
            let rust_type = munch_type(&mut lexer)?;
            lexer.munch_end()?;
            Some(rust_type)
        },
        Some(_) => return Err((lexer.column(), ParseErrorBody::Expected(": or end of line")))
    };

    Ok(SerialTupleItem {
        value,
        rust_type,
        value_children: children
    })
}

fn parse_rust_type(line: &str) -> Result<SerialRustType, (usize, ParseErrorBody)> {
    let mut lexer = Lexer::<GraphToken>::new(line);
    let rust_type = munch_type(&mut lexer)?;
    lexer.munch_end()?;
    Ok(rust_type)
}

fn parse_ident(line: &str) -> Result<String, (usize, ParseErrorBody)> {
    let mut lexer = Lexer::<GraphToken>::new(line);
    lexer.munch("ident", |token| extract!(token, GraphToken::Ident))?;
    Ok(lexer.slice().to_string())
}

fn munch_value_or_underscore(lexer: &mut Lexer<GraphToken>) -> Result<Option<SerialValueHead>, (usize, ParseErrorBody)> {
    // lexer doesn't support peek but there are easy workarounds, here is one
    let remaining_chars = lexer.remainder().trim_start().chars();
    if remaining_chars.first() == Some('_') && !remaining_chars.get(1).map_or(false, |c| c.is_alphanumeric()) {
        lexer.munch("'_'", |token| extract!(token, GraphToken::Punct('_'))).expect("peek failed");
        Ok(None)
    } else {
        Ok(Some(munch_value(lexer)?))
    }
}

fn munch_value(lexer: &mut Lexer<GraphToken>) -> Result<SerialValueHead, (usize, ParseErrorBody)> {
    match lexer.next() {
        None => Err((lexer.span().end, ParseErrorBody::ExpectedMore("value"))),
        Some(GraphToken::Integer(int)) => match int {
            None => Err((lexer.span().start, ParseErrorBody::BadNumber)),
            Some(int) => Ok(SerialValueHead::Integer(int))
        }
        Some(GraphToken::Float(float)) => match float {
            None => Err((lexer.span().start, ParseErrorBody::BadNumber)),
            Some(float) => Ok(SerialValueHead::Float(float))
        }
        Some(GraphToken::String) => match unescape(lexer.slice()) {
            Err(error) => Err((lexer.span().start, ParseErrorBody::BadEscape(error))),
            Ok(string) => Ok(SerialValueHead::String(string))
        }
        Some(GraphToken::Ident) => {
            let node_ident = lexer.slice().to_string();
            lexer.munch("'.'", |token| extract!(token, GraphToken::Punct('.')))?;
            lexer.munch("ident", |token| extract!(token, GraphToken::Ident))?;
            let field_ident = lexer.slice().to_string();
            Ok(SerialValueHead::Ref {
                node_ident,
                field_ident
            })
        }
        Some(GraphToken::Punct('[')) => {
            let mut values = Vec::new();
            loop {
                match munch_value(lexer) {
                    Err((column, error)) => match error {
                        ParseErrorBody::Unopened(']') => break,
                        ParseErrorBody::ExpectedMore(_) => return Err((column, ParseErrorBody::Unclosed('['))),
                        _ => return Err((column, error))
                    }
                    Ok(value) => {
                        values.push(value);
                        lexer.munch("','", |token| extract!(token, GraphToken::Punct(',')))?;
                    }
                }
            }
            Ok(SerialValueHead::Array(values))
        }
        Some(GraphToken::Punct('(')) => {
            let mut values = Vec::new();
            loop {
                match munch_value(lexer) {
                    Err((column, error)) => match error {
                        ParseErrorBody::Unopened(')') => break,
                        ParseErrorBody::ExpectedMore(_) => return Err((column, ParseErrorBody::Unclosed('('))),
                        _ => return Err((column, error))
                    }
                    Ok(value) => {
                        values.push(value);
                        lexer.munch("','", |token| extract!(token, GraphToken::Punct(',')))?;
                    }
                }
            }
            Ok(SerialValueHead::Tuple(values))
        }
        // Special error message because we catch this one when we do actually have an opened bracket
        Some(GraphToken::Punct(']')) => Err((lexer.span().start, ParseErrorBody::Unopened(']'))),
        Some(GraphToken::Punct(')')) => Err((lexer.span().start, ParseErrorBody::Unopened(')'))),
        Some(_) => Err((lexer.span().start, ParseErrorBody::Expected("value")))
    }
}

fn munch_type(lexer: &mut Lexer<GraphToken>) -> Result<SerialRustType, (usize, ParseErrorBody)> {
    match lexer.next() {
        None => Err((lexer.span().end, ParseErrorBody::ExpectedMore("type"))),
        Some(GraphToken::Ident) => {
            let name = lexer.slice().to_string();
            let mut generic_args = Vec::new();
            // lexer doesn't support peek but there are easy workarounds, here is one
            if lexer.remainder().trim_start().chars().first() == Some('<') {
                lexer.munch("'<'", |token| extract!(token, GraphToken::Punct('<'))).expect("peek failed");
                loop {
                    match munch_type(lexer) {
                        Err((column, error)) => match error {
                            ParseErrorBody::Unopened('>') => break,
                            ParseErrorBody::ExpectedMore(_) => return Err((column, ParseErrorBody::Unclosed('<'))),
                            _ => return Err((column, error))
                        }
                        Ok(generic_arg) => {
                            generic_args.push(generic_arg);
                            lexer.munch("','", |token| extract!(token, GraphToken::Punct(',')))?;
                        }
                    }
                }
            }

            Ok(SerialRustType::Ident {
                name,
                generic_args
            })
        }
        Some(GraphToken::Punct('&')) => {
            let refd = Box::new(munch_type(lexer)?);
            Ok(SerialRustType::Ref(refd))
        }
        Some(GraphToken::Punct('[')) => {
            let elem = Box::new(munch_type(lexer)?);
            match lexer.next() {
                None => Err((lexer.span().end, ParseErrorBody::ExpectedMore("';' or ']'"))),
                Some(GraphToken::Punct(';')) => {
                    let length = lexer.munch("integer", |token| extract!(token, GraphToken::Integer(int)))?;
                    lexer.munch("']'", |token| extract!(token, GraphToken::Punct(']')))?;
                    Ok(SerialRustType::Array {
                        elem,
                        length
                    })
                }
                Some(GraphToken::Punct(']')) => Ok(SerialRustType::Slice(elem)),
                Some(_) => Err((lexer.span().start, ParseErrorBody::Expected("';' or ']'")))
            }
        }
        Some(GraphToken::Punct('(')) => {
            let mut types = Vec::new();
            loop {
                match munch_type(lexer) {
                    Err((column, error)) => match error {
                        ParseErrorBody::Unopened(')') => break,
                        ParseErrorBody::ExpectedMore(_) => return Err((column, ParseErrorBody::Unclosed('('))),
                        _ => return Err((column, error))
                    }
                    Ok(value) => {
                        types.push(value);
                        lexer.munch("','", |token| extract!(token, GraphToken::Punct(',')))?;
                    }
                }
            }
            Ok(SerialRustType::Tuple(types))
        }
        // Special error message because we catch this one when we do actually have an opened bracket
        Some(GraphToken::Punct(']')) => Err((lexer.span().start, ParseErrorBody::Unopened(']'))),
        Some(GraphToken::Punct(')')) => Err((lexer.span().start, ParseErrorBody::Unopened(')'))),
        Some(_) => Err((lexer.span().start, ParseErrorBody::Expected("value")))
    }
}

fn expect_no_lines(p: &mut GraphParser, lines: &[(usize, String)]) {
    if !lines.is_empty() {
        p.errors.push(ParseError {
            path: p.path.to_path_buf(),
            line: *lines.first().unwrap().0,
            column: 0,
            body: ParseErrorBody::ExpectedLess
        })
    }
}