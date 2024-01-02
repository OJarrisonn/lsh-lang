use std::fmt::Display;

use crate::parse::expression::{Symbol, Expression};

#[derive(Debug)]
pub enum LSHErrorKind {
    Generic,
    Arguments,
    Symbol,
    Type,
    Macro
}

#[derive(Debug)]
pub struct LSHError {
    pub kind: LSHErrorKind,
    pub msg: String
}

impl Display for LSHErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}Error", match self {
            Self::Generic => "Generic",
            Self::Arguments => "ArgumentMismatch",
            Self::Symbol => "Symbol",
            Self::Type => "Type",
            Self::Macro => "Macro"
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

    pub fn undefined_symbol(symbol: Symbol) -> Self {
        Self {
            kind: LSHErrorKind::Symbol,
            msg: format!("Symbol {symbol} is not defined")
        }
    }

    pub fn not_a_macro(expr: Expression) -> Self {
        Self {
            kind: LSHErrorKind::Type,
            msg: format!("The provided expression {expr} is not a macro", )
        }
    }

    pub fn not_a_function(expr: Expression) -> Self {
        Self {
            kind: LSHErrorKind::Type,
            msg: format!("The provided expression {expr} is not a function", )
        }
    }

    pub fn unexpected_arg_type(arg: Symbol) -> Self {
        Self {
            kind: LSHErrorKind::Arguments,
            msg: format!("The provided argument for {arg} doesn't fit the expectations")
        }
    }

    pub fn macro_remainder_not_appliable(expr: Expression) -> Self {
        Self {
            kind: LSHErrorKind::Macro,
            msg : format!("The expression {expr} can't contain a macro remainder")
        }
    }
}