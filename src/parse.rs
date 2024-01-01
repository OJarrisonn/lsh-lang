use pest::Parser;
use pest_derive::Parser;

use crate::eval::symbol_table::SymbolTable;

use self::expression::Expression;

pub mod expression;

#[derive(Parser)]
#[grammar = "lsh.pest"]
pub struct LSHParser;

/// Receives a &str and parses it to a PestPairs strucuture if it's correct, or to a PestError if not
/// This parses the raw input as a program
pub fn parse<'a>(source: &'a str) -> Result<Expression, pest::error::Error<Rule>>{
    let crude_parse = LSHParser::parse(Rule::expr, source);

    if let Err(e) = crude_parse {
        return Err(e);
    }

    let pairs = crude_parse.expect("Err() got treated before");
    
    let expr = Expression::List(
        pairs.map(|p| Expression::from(p))
            .collect());

    Ok(expr)
}