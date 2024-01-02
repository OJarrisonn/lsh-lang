use crate::{eval::symbol_table::SymbolTable, parse::expression::Expression, error::LSHError};

pub fn print(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    for arg in args {
        print!("{arg}");
    }
    println!("");
    Ok(Expression::Nil)
}