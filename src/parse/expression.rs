use std::{fmt::Display, iter::zip};

use pest::iterators::Pair;

use crate::{eval::{symbol_table::SymbolTable, eval}, error::LSHError};

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

pub type NativeMacro = fn(&mut SymbolTable, Vec<Expression>) -> Result<Expression, LSHError>;

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
            Rule::string => Expression::String((&(value.as_span().as_str())[1..(value.as_span().as_str().len()-1)]).to_string()),
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

impl Macro {
    fn apply_remainder(expr: Expression, remainder: Vec<Expression>) -> Expression {
        match expr {
            Expression::List(list) => Expression::List(Macro::replace_remainder(list, remainder)),
            Expression::DataList(list) => Expression::DataList(Macro::replace_remainder(list, remainder)),
            Expression::SymbolList(list) => Expression::SymbolList(
                Macro::replace_remainder(list.into_iter().map(|s| Expression::Symbol(s)).collect(), remainder).into_iter()
                    .map(|e| match e {
                        Expression::Symbol(s) => s,
                        e => {
                            eprintln!("Unexpected not symbol in macro remainder replacing. {e}");
                            Symbol::Identifier("".to_string())
                        }
                    }).collect()
            ),
            Expression::Call(call) => match call {
                Call::FunctionCall(s, args) => Expression::Call(Call::FunctionCall(s, Macro::replace_remainder(args, remainder))),
                Call::MacroCall(s, args) => Expression::Call(Call::MacroCall(s, Macro::replace_remainder(args, remainder))),
            },
            Expression::Function(func) => match func {
                Function::Native(_) => panic!("You shouldn't be able to call an apply_remainder in a NativeFunction definition"),
                Function::Defined(defined) => {
                    Expression::Function(
                        Function::Defined(DefinedFunction { 
                            params: Macro::replace_remainder(defined.params.into_iter().map(|s| Expression::Symbol(s)).collect(), remainder.clone()).into_iter()
                            .map(|e| match e {
                                Expression::Symbol(s) => s,
                                e => {
                                    eprintln!("Unexpected not symbol in macro remainder replacing. {e}");
                                    Symbol::Identifier("".to_string())
                                }
                            }).collect(), 
                            body: Box::new(Macro::apply_remainder(*defined.body, remainder)) 
                        })
                    )
                },
            },
            Expression::Macro(_) => panic!("No nested macro definition allowed"),
            expr => expr
        }
    }

    fn replace_remainder(list: Vec<Expression>, remainder: Vec<Expression>) -> Vec<Expression> {
        let mut list = list;
        
        let mut id = 0;
        while id < list.len() {
            if let Expression::MacroRemainder = list[id] {
                list.splice(id..=id, remainder.clone());
                id += remainder.len();
            } else {
                id += 1;
            }
        }

        list
    }

    fn replace_symbol(expr: Expression, symbol: &Symbol, apply: &Expression) -> Expression {
        match expr {
            Expression::List(list) => Expression::List(list.into_iter()
                .map(|e| Macro::replace_symbol(e, symbol, apply))
                .collect()),
            Expression::DataList(list) => Expression::DataList(list.into_iter()
                .map(|e| Macro::replace_symbol(e, symbol, apply))
                .collect()),
            Expression::Call(call) => Expression::Call(match call {
                Call::FunctionCall(s, args) => Call::FunctionCall(s, args.into_iter()
                    .map(|e| Macro::replace_symbol(e, symbol, apply))
                    .collect()),
                Call::MacroCall(s, args) => Call::MacroCall(s, args.into_iter()
                    .map(|e| Macro::replace_symbol(e, symbol, apply))
                    .collect()),
            }),
            Expression::Symbol(local) => if local == *symbol { apply.clone() } else { Expression::Symbol(local) },
            expr => expr
        }
    }

    pub fn exec(&self, table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
        match self {
            Macro::Native(native) => {
                native(table, args)
            },
            Macro::Defined(defined) => {
                let remainder_len = args.len() as isize - defined.params.len() as isize;

                if remainder_len < 0 {
                    return Err(LSHError::wrong_arg_count(defined.params.len(), args.len()));
                } 
                
                let mut body = (*defined.body).clone();

                for (symbol, apply) in zip(&defined.params, &args[..(remainder_len as usize)]) {
                    body = Macro::replace_symbol(body, symbol, apply);
                }

                if remainder_len != 0 {
                    let remainder = (&args[defined.params.len()..]).to_vec();
                    
                    body = Macro::apply_remainder(body, remainder);
                }

                eval(table, body)
            },
        }
    }
}