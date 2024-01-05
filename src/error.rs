use std::fmt::Display;

use crate::parse::expression::{Symbol, Expression};

#[derive(Debug)]
pub enum LSHRuntimeErrorKind<'a> {
    Generic(String), // A message explaining what happened
    UndefinedIdentifier(&'a Symbol), // Which symbol is undefined
    MutatingImmutable(&'a Symbol), // Which symbol was violated
    NotCallable(&'a Expression), // The expression that isn't callable
    TooManyArguments(usize, usize), // The expected amount, the amount got
    IncompatibleArgument(&'a Expression, String) // The argument got, and a message of how to fix it
}

#[derive(Debug)]
pub struct LSHRuntimeErrorStack<'a> {
    kind: Option<LSHRuntimeErrorKind<'a>>,
    from: Option<Box<LSHRuntimeErrorStack<'a>>>,
    place: &'a str,
    point: (usize, usize),
    note: Option<String>
}

impl Display for LSHRuntimeErrorKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Generic(msg) => format!("GenericError: {msg}"),
            Self::UndefinedIdentifier(symbol) => format!("UndefinedIdentifier: {symbol} isn't defined in the current scope or in any other level above"),
            Self::MutatingImmutable(symbol) => format!("MutatingImmutable: Trying to mutate {symbol} which is immutable"),
            Self::NotCallable(expr) => format!("NotCallable: Trying to call an expression that isn't callable of type {}", expr.type_name()),
            Self::TooManyArguments(expected, got) => format!("TooManyArguments: Passed {got} arguments to a call that expected {expected}"),
            Self::IncompatibleArgument(expr, msg) => format!("IncompatibleArgument: The expression `{expr}` was passed as an argument to a call, but it's not acceptable. {}", msg)
        })
    }
}

impl<'a> Display for LSHRuntimeErrorStack<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.kind.is_some() {
            write!(f, "{}\nAt {}, {} :: {} {}",
                self.kind.unwrap(), 
                self.point.0, 
                self.point.1, 
                self.place, 
                if self.note.is_some() {
                    self.note.unwrap()
                } else {
                    String::new()
                })
        } else {
            write!(f, "{}\nAt {}, {} :: {} {}", 
                self.from.unwrap(), 
                self.point.0, 
                self.point.1, 
                self.place, 
                if self.note.is_some() {
                    self.note.unwrap()
                } else {
                    String::new()
                })
        }
    }
}

impl<'a> LSHRuntimeErrorStack<'a> {
    pub fn create_source(place: &'a str, point: (usize, usize), kind: LSHRuntimeErrorKind<'a>) -> Self {
        Self {
            place,
            point,
            kind: Some(kind), 
            from: None,
            note: None
        }
    }

    pub fn create_stream(place: &'a str, point: (usize, usize), from: LSHRuntimeErrorStack<'a>) -> Self {
        Self {
            place,
            point,
            kind: None,
            from: Some(Box::new(from)),
            note: None
        }
    }

    pub fn append_note(self, note: String) -> Self {
        Self {
            note: Some(note),
            ..self
        }
    }
}
