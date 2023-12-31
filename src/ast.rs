use std::path::Path;

pub enum Expr {

}

pub enum Symbol {
    Identifier(String),
    Operator(String),
    Macro(String)
}

pub enum Literal {
    Number(Number),
    Path(String),
    String(String),
    Bool(bool)
}

pub enum Number {
    Integer(i64),
    Float(f64)
}