use pest::iterators::Pairs;

use crate::{symbol_table::SymbolTable, parse::{Rule, Expression}, error::LSHError};

pub fn eval(st: &mut SymbolTable, ast: Pairs<Rule>) -> Result<Expression, LSHError> {
    Ok(Expression::Nil)
}