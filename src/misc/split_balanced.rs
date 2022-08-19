use smallvec::SmallVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParenType {
    open: char,
    close: char,
}

impl ParenType {
    pub const PAREN: ParenType = ParenType {
        open: '(',
        close: ')',
    };

    pub const BRACKET: ParenType = ParenType {
        open: '[',
        close: ']',
    };

    pub const BRACE: ParenType = ParenType {
        open: '{',
        close: '}',
    };

    pub const ANGLE: ParenType = ParenType {
        open: '<',
        close: '>'
    };

    pub const ALL_IN_RUST: [ParenType; 4] = [
        ParenType::PAREN,
        ParenType::BRACKET,
        ParenType::BRACE,
        ParenType::ANGLE
    ];
}

pub struct Iter<'a, 'b> {
    str: &'a str,
    delimiter: char,
    paren_types: &'b [ParenType],
}

pub trait SplitBalanced {
    /// Split on delimiter, but not when in one of the given parenthesis types
    fn split_balanced<'a, 'b>(&'a self, delimiter: char, paren_types: &'b [ParenType]) -> Iter<'a, 'b>;
}

impl SplitBalanced for str {
    fn split_balanced<'a, 'b>(&'a self, delimiter: char, paren_types: &'b [ParenType]) -> Iter<'a, 'b> {
        Iter::new(self, delimiter, paren_types)
    }
}

impl<'a, 'b> Iter<'a, 'b> {
    fn new(str: &'a str, delimiter: char, paren_types: &'b [ParenType]) -> Self {
        Iter {
            str,
            delimiter,
            paren_types
        }
    }
}

impl<'a, 'b> Iterator for Iter<'a, 'b> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.str.is_empty() {
            return None;
        }

        let mut level = SmallVec::<[char; 8]>::new();
        for (idx, char) in self.str.char_indices() {
            if char == self.delimiter && level.is_empty() {
                let (left, right) = self.str.split_at(idx);
                // Skip over char
                self.str = &right[1..];
                return Some(left);
            } else if level.last().copied() == Some(char) {
                level.pop();
            } else {
                for paren_type in self.paren_types {
                    if char == paren_type.open {
                        level.push(char);
                    }
                }
            }
        }

        // Reached end
        let left = self.str;
        self.str = "";
        Some(left)
    }
}