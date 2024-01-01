use pest::iterators::Pairs;

use crate::{parse::{Rule, expression::Expression}, error::LSHError};

use self::symbol_table::SymbolTable;

pub mod symbol_table;

pub fn eval(st: &mut SymbolTable, ast: Pairs<Rule>) -> Result<Expression, LSHError> {
    Ok(Expression::Nil)
}