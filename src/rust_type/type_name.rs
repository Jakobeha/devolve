use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use join_lazy_fmt::Join;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RustTypeName {
    pub qualifier: String,
    pub simple_name: String,
    pub generic_args: Vec<RustTypeName>
}

pub struct RustTypeNameQualified<'a>(&'a RustTypeName);

pub struct RustTypeNameUnqualified<'a>(&'a RustTypeName);

pub struct RustTypeNameUnambiguous<'a> {
    type_name: &'a RustTypeName,
    must_qualify: bool
}

impl RustTypeName {
    pub fn simple(qualifier: String, simple_name: String) -> RustTypeName {
        RustTypeName {
            qualifier,
            simple_name,
            generic_args: Vec::new()
        }
    }

    pub fn anonymous(desc: &str) -> RustTypeName {
        RustTypeName {
            qualifier: "".to_string(),
            simple_name: format!("{{{}}}", desc),
            generic_args: Vec::new()
        }
    }

    pub fn unknown() -> RustTypeName {
        RustTypeName::anonymous("unknown")
    }

    pub fn bottom() -> RustTypeName {
        RustTypeName::anonymous("bottom")
    }

    pub fn is_unknown(&self) -> bool {
        self.qualifier == "" && self.simple_name == "unknown"
    }

    pub fn is_bottom(&self) -> bool {
        self.qualifier == "" && self.simple_name == "bottom"
    }

    pub fn is_anonymous(&self) -> bool {
        self.simple_name.starts_with('{') && self.simple_name.ends_with('}')
    }

    pub fn qualified(&self) -> RustTypeNameQualified<'_> {
        RustTypeNameQualified(self)
    }

    pub fn unqualified(&self) -> RustTypeNameUnqualified<'_> {
        RustTypeNameUnqualified(self)
    }

    /// Qualifies the type name if its simple name is in the set
    pub fn display(&self, simple_names_in_scope: &HashSet<String>) -> RustTypeNameUnambiguous<'_> {
        RustTypeNameUnambiguous {
            type_name: self,
            must_qualify: simple_names_in_scope.contains(&self.simple_name)
        }
    }
}

pub struct RustTypeNameParseError {
    index: usize
}

impl TryFrom<String> for RustTypeName {
    type Error = RustTypeNameParseError;

    fn try_from(mut value: String) -> Result<Self, Self::Error> {
        let qualifier_index = value.find("::");
        let params_index = value.find('<');
        let (mut qualifier, mut name_and_generics) = match qualifier_index {
            None => (String::new(), value),
            Some(index) => {
                let rest = value.split_off(index + 2);
                let mut qualifier = value;
                qualifier.truncate(index);

                (qualifier, rest)
            }
        };
        let (mut simple_name, generic_args) = match params_index {
            None => (name_and_generics, Vec::new()),
            Some(index) => {
                let mut generics_str = name_and_generics.split_off(index + 1);
                let mut name = name_and_generics;
                name.truncate(index);
                if !generics_str.ends_with('>') {
                    return Err(RustTypeNameParseError { index: generics_str.len() - 1 });
                }
                generics_str.truncate(generics_str.len() - 1);

                let mut generic_args = Vec::new();
                while let Some(comma_index) = generics_str.find(',') {
                    let params_str = generics_str.split_off(comma_index + 1);
                    let mut param = params_str;
                    param.truncate(comma_index);
                    generic_args.push(RustTypeName::try_from(param)?);
                }

                (name, generic_args)
            }
        };

        qualifier.trim_in_place();
        simple_name.trim_in_place();

        Ok(RustTypeName {
            qualifier,
            simple_name,
            generic_args
        })
    }
}

impl Display for RustTypeNameQualified<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.0.qualifier.is_empty() {
            write!(f, "{}::", self.0.qualifier)
        }
        write!(f, "{}", self.0.simple_name)?;
        if !self.generic_args.is_empty() {
            write!(f, "<{}>", ", ".join(self.0.generic_args.qualified()))?;
        }
        Ok(())
    }
}

impl Display for RustTypeNameUnqualified<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.simple_name)?;
        if !self.generic_args.is_empty() {
            write!(f, "<{}>", ", ".join(self.0.generic_args.unqualified()))?;
        }
        Ok(())
    }
}

impl Display for RustTypeNameUnambiguous<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.must_qualify {
            write!(f, "{}", self.type_name.qualified())
        } else {
            write!(f, "{}", self.type_name.unqualified())
        }
    }
}

trait StringExt {
    fn trim_in_place(&mut self);
}

impl StringExt for String {
    fn trim_in_place(&mut self) {
        let trim_off_end = self.len() - self.trim_end().len();
        let trim_off_start = self.len() - self.trim_start().len();

        self.drain(..trim_off_start);
        self.drain(trim_off_end..);
    }
}