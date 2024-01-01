use crate::{parse::expression::{Expression, Symbol}, error::LSHError};

use super::symbol_table::SymbolTable;

pub fn bin_add(table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match a {
            Expression::Integer(a) => match b {
                Expression::Integer(b) => Ok(Expression::Integer(a + b)),
                Expression::Float(b) => Ok(Expression::Float(*a as f64 + b)),
                _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("b".to_string())))
        },
            Expression::Float(a) => match b {
                Expression::Integer(b) => Ok(Expression::Float(a + *b as f64)),
                Expression::Float(b) => Ok(Expression::Float(a + b)),
                _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("b".to_string())))
        },
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a".to_string())))
        }
    }
}
