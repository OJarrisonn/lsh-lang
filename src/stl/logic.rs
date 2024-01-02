use crate::{error::LSHError, parse::expression::{Expression, Symbol}, eval::symbol_table::SymbolTable};

pub fn is_true(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 1 {
        return Err(LSHError::wrong_arg_count(1, args.len()));
    } 

    match &args[0] {
        Expression::Nil => Ok(Expression::Bool(false)),
        Expression::Integer(i) => Ok(Expression::Bool(*i != 0)),
        Expression::Float(f) => Ok(Expression::Bool(*f != 0.0)),
        Expression::Bool(b) => Ok(Expression::Bool(*b)),
        Expression::String(s) => Ok(Expression::Bool(!s.is_empty())),
        _ => Ok(Expression::Bool(false)),
    }
}

pub fn is_false(table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    match is_true(table, args) {
        Ok(Expression::Bool(b)) => Ok(Expression::Bool(!b)),
        e => e,
    }
}

pub fn tern_if(table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 3 {
        return Err(LSHError::wrong_arg_count(3, args.len()));
    }

    let cond = args[0].clone();
    let truth = args[1].clone();
    let fake = args[2].clone();

    match is_true(table, vec![cond]) {
        Ok(Expression::Bool(true)) => Ok(truth),
        Ok(Expression::Bool(false)) => Ok(fake),
        e => e
    } 
}

pub fn bin_gt(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        Err(LSHError::wrong_arg_count(2, args.len()))
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Bool(a > b)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Bool(*a > *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Bool(*a as f64 > *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Bool(a > b)),
            _ => Err(LSHError::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}