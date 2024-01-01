use std::fmt::Display;

pub enum LSHErrorKind {
    Generic,
    Arguments,
}

pub struct LSHError {
    pub kind: LSHErrorKind,
    pub msg: String
}

impl Display for LSHErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}Error", match self {
            Self::Generic => "Generic",
            Self::Arguments => "ArgumentMismatch"
        })
    }
}

impl Display for LSHError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
    }
}

impl LSHError {
    pub fn wrong_arg_count(expected: usize, got: usize) -> Self {
        Self {
            kind: LSHErrorKind::Arguments,
            msg: format!("Expected {expected}, but got {got}"),
        }
    }
}