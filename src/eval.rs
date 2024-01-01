use pest::iterators::Pairs;

use crate::{parse::{Rule, expression::{Expression, Call}}, error::LSHError};

use self::symbol_table::SymbolTable;

pub mod symbol_table;
mod native_functions;

pub fn eval(table: &mut SymbolTable, expr: Expression) -> Result<Expression, LSHError> {
    match expr {
        Expression::List(list) => {
            let mut results: Vec<_> = list.into_iter()
                .map(|expr| eval(table, expr))
                .collect();

            match results.pop() {
                Some(last) => last,
                None => Ok(Expression::Nil)
            }
        },
        Expression::DataList(list) => {
            let mut failure: Option<LSHError> = None;
            let results: Vec<Expression> = list.into_iter()
                .map(|expr| eval(table, expr))
                .map(|res| match res {
                    Ok(expr) => expr,
                    Err(e) => {
                        eprintln!("{e}");
                        
                        if failure.is_none() {
                            failure = Some(e);
                        }

                        Expression::Nil
                    }
                })
                .collect();

            match failure {
                Some(e) => Err(e),
                None => Ok(Expression::DataList(results))
            }
        },
        Expression::Call(call) => {
            match call {
                Call::FunctionCall(symbol, args) => {
                    let func = eval(table,Expression::Symbol(symbol));
                    let args = eval(table, Expression::DataList(args));

                    if let Err(e) = args {
                        return Err(e);
                    }

                    let args = args.unwrap().into();
                    
                    match func {
                        Err(e) => Err(e),
                        Ok(func) => match func {
                            Expression::Function(func) => func.exec(table, args),
                            _ => Err(LSHError::not_a_function(func))
                        }
                    }

                },
                Call::MacroCall(symbol, args) => todo!(),
            }
        },
        Expression::Function(_) => todo!(),
        Expression::Macro(_) => todo!(),
        Expression::Symbol(symbol) => {
            match table.get(&symbol) {
                Some(value) => Ok(value),
                None => Err(LSHError::undefined_symbol(symbol)),
            }
        },
        expr => Ok(expr)
    }
}