use crate::{error::LSHError, eval::symbol_table::SymbolTable, parse::expression::{Expression, Symbol}};

pub fn def(table: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHError> {
    if args.len() != 2 {
        return Err(LSHError::wrong_arg_count(2, args.len()));
    }

    let symbol = args[0].clone();
    let value = args[1].clone();

    if let Expression::Symbol(symbol) = symbol {
        if let Symbol::MacroIdentifier(_) = symbol { 
            if let Expression::Macro(_) = value { 
                table.set(symbol, value);

                Ok(Expression::Nil)
            } else {
                Err(LSHError::unexpected_arg_type(Symbol::Identifier("value".to_string())))
            }
        } else {
            table.set(symbol, value);

            Ok(Expression::Nil)
        }
    } else {
        Err(LSHError::unexpected_arg_type(Symbol::Identifier("name".to_string())))
    }

}