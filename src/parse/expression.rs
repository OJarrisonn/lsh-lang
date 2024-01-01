use std::{fmt::Display, iter::zip};

use pest::iterators::Pair;

use crate::{eval::{symbol_table::SymbolTable, self, eval}, error::LSHError};

use super::Rule;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Symbol {
    Identifier(String),
    MacroIdentifier(String)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Nil,
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    List(Vec<Expression>),
    DataList(Vec<Expression>),
    SymbolList(Vec<Symbol>),
    Call(Call),
    Function(Function),
    Macro(Macro),
    Symbol(Symbol),
    MacroRemainder,
}

#[derive(Debug, Clone)]
pub enum Call {
    FunctionCall(Symbol, Vec<Expression>),
    MacroCall(Symbol, Vec<Expression>)
}

#[derive(Debug, Clone)]
pub enum Function {
    Native(NativeFunction),
    Defined(DefinedFunction)
}

#[derive(Debug, Clone)]
pub enum Macro {
    Native(NativeMacro),
    Defined(DefinedMacro)
}


pub type NativeFunction = fn(&mut SymbolTable, Vec<Expression>) -> Result<Expression, LSHError>;

#[derive(Debug, Clone)]
pub struct DefinedFunction {
    params: Vec<Symbol>,
    body: Box<Expression>
}

pub type NativeMacro = fn(Vec<Expression>) -> Result<Expression, LSHError>;

#[derive(Debug, Clone)]
pub struct DefinedMacro {
    params: Vec<Symbol>,
    body: Box<Expression>
}


impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Symbol::Identifier(s) => s,
            Symbol::MacroIdentifier(s) => s
        })
    }
}

impl From<Pair<'_, Rule>> for Symbol {
    fn from(value: Pair<'_, Rule>) -> Self {
        if value.as_rule() != Rule::identifier || value.as_rule() != Rule::macro_identifier {
        }
        
        match value.as_rule() {
            Rule::identifier => Self::Identifier(value.as_span().as_str().to_string()),
            Rule::macro_identifier => Self::MacroIdentifier(value.as_span().as_str().to_string()),
            _ => panic!("Can't get symbol out of not a symbol")
        }
    }
}

impl Expression {
    pub fn same_type(&self, other: &Self) -> bool {
        self.type_name() == other.type_name()
    }

    pub fn type_name(&self) -> String {
        format!("{}", match self {
            Expression::Nil => "nil",
            Expression::Integer(_) => "int",
            Expression::Float(_) => "float",
            Expression::Bool(_) => "bool",
            Expression::String(_) => "str",
            Expression::List(_) => "list",
            Expression::DataList(_) => "data-list",
            Expression::SymbolList(_) => "symbol-list",
            Expression::Call(_) => "call",
            Expression::Function(_) => "function",
            Expression::Macro(_) => "macro",
            Expression::Symbol(_) => "symbol",
            Expression::MacroRemainder => "..."
        })
    }

    pub fn get_symbol_list(&self) -> &Vec<Symbol> {
        match self {
            Expression::SymbolList(list) => list,
            _ => panic!("Can't get symbol list from {}", self.type_name())
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Expression::Nil => "nil".to_string(),
            Expression::Integer(i) => i.to_string(),
            Expression::Float(f) => f.to_string(),
            Expression::Bool(b) => b.to_string(),
            Expression::String(s) => s.to_string(),
            Expression::List(l) => {
                let mut s = "(".to_string();

                for e in l {
                    s = format!("{s} {e}");
                }

                format!("{s})")
            },
            Expression::DataList(l) => {
                let mut s = "[".to_string();

                for e in l {
                    s = format!("{s} {e}");
                }

                format!("{s}]")
            },
            Expression::SymbolList(l) => {
                let mut s = "(s: ".to_string();

                for e in l {
                    s = format!("{s} {e}");
                }

                format!("{s})")
            },
            Expression::Call(c) => match c {
                Call::FunctionCall(s, _) => format!("(call {s})"),
                Call::MacroCall(s, _) => format!("(call {s})")
            },
            Expression::Function(f) => match f {
                Function::Defined(l) => format!("(lambda {:?})", &l.params),
                Function::Native(_) => format!("(native)")
            },
            Expression::Macro(_) => format!("(macro)"),
            Expression::Symbol(s) => s.to_string(),
            Expression::MacroRemainder => "...".to_string()
        })
    }
}

impl From<Pair<'_, Rule>> for Expression {
    fn from(value: Pair<'_, Rule>) -> Self {
        match value.as_rule() {
            Rule::expr => Self::from(value.into_inner().next().unwrap()),
            Rule::identifier => Self::Symbol(
                Symbol::Identifier(
                    value.as_span().as_str().to_string()
                )
            ),
            Rule::macro_identifier => Self::Symbol(
                Symbol::MacroIdentifier(
                    value.as_span().as_str().to_string()
                )
            ),
            Rule::list => Expression::List(value.into_inner()
                                            .map(|expr| Self::from(expr))
                                            .collect()),
            Rule::data_list => Expression::DataList(value.into_inner()
                                            .map(|expr| Self::from(expr))
                                            .collect()),
            Rule::ident_list => Expression::SymbolList(value.into_inner()
                                            .map(|expr| Symbol::from(expr))
                                            .collect()),
            Rule::call => {
                let mut value: Vec<Pair<'_, Rule>> = value.into_inner().collect();
                let head = value.remove(0);

                match head { // Check the first element in the call and make the right Call expression
                    symb if Rule::identifier == symb.as_rule() => {
                        let ident = Symbol::Identifier(symb.as_span().as_str().to_string());
                        let args = value.into_iter()
                            .map(|p| Expression::from(p))
                            .collect();


                        Expression::Call(Call::FunctionCall(ident, args))
                    },
                    symb if Rule::macro_identifier == symb.as_rule() => {
                        let ident = Symbol::MacroIdentifier(symb.as_span().as_str().to_string());
                        let args = value.into_iter()
                            .map(|p| Expression::from(p))
                            .collect();


                        Expression::Call(Call::MacroCall(ident, args))
                    },
                    value => panic!("First element of a call must be a symbol, not a {value}")
                }
                
            },
            Rule::function => {
                let mut value = value.into_inner();
                let params = Expression::from(value.next().unwrap()).get_symbol_list().clone();
                let body = Box::new(Expression::from(value.next().unwrap()));

                Expression::Function(Function::Defined(DefinedFunction { params , body }))
            },
            Rule::r#macro => {
                let mut value = value.into_inner();
                let params = Expression::from(value.next().unwrap()).get_symbol_list().clone();
                let body = Box::new(Expression::from(value.next().unwrap()));

                Expression::Macro(Macro::Defined(DefinedMacro { params , body }))
            },
            Rule::macro_remainder => Expression::MacroRemainder,
            Rule::float => Expression::Float(value.as_span().as_str().parse().unwrap()),
            Rule::integer => Expression::Integer(value.as_span().as_str().parse().unwrap()),
            Rule::string => Expression::String(value.as_span().as_str().to_string()),
            Rule::bool => Expression::Bool(value.as_span().as_str().parse().unwrap()),
            _ => panic!("Rule {:?} can't be converted to a value. In {}", value.as_rule(), value.as_str())
        }
    }
}

impl Into<Vec<Expression>> for Expression {
    fn into(self) -> Vec<Expression> {
        match self {
            Expression::List(list) | Expression::DataList(list) => list,
            Expression::SymbolList(list) => list.into_iter().map(|symbol| Expression::Symbol(symbol)).collect(),
            expr => vec![expr]
        }
    }
}

impl Function {
    pub fn exec(&self, table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
        match self {
            Function::Native(native) => native(table, args),
            Function::Defined(defined) => {
                if args.len() != defined.params.len() {
                    Err(LSHError::wrong_arg_count(defined.params.len(), args.len()))
                } else {
                    zip(defined.params.clone(), args).for_each(|(symbol, expression)| table.set(symbol, expression));

                    eval(table, *defined.body.clone())
                }
            },
        }
    }
}
