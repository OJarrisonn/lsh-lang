use crate::{parse::expression::{Expression, Symbol}, error::LSHError, eval::symbol_table::SymbolTable};

pub fn bin_add(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Integer(a + b)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a + *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 + *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a + b)),
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}

pub fn bin_sub(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Integer(a - b)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a - *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 - *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a - b)),
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}

pub fn bin_mul(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Integer(a * b)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a * *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 * *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a * b)),
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}
pub fn bin_div(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Float(*a as f64 / *b as f64)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a / *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 / *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a / b)),
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}