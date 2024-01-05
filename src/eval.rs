use pest::iterators::Pairs;

use crate::{parse::{Rule, expression::{Expression, Call, Function}}, error::{LSHRuntimeErrorStack, LSHRuntimeErrorKind}};

use self::symbol_table::SymbolTable;

pub mod symbol_table;

pub fn eval(table: &mut SymbolTable, expr: Expression) -> Result<Expression, LSHRuntimeErrorStack> {
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
            let mut failure: Option<LSHRuntimeErrorStack> = None;
            let results: Vec<Expression> = list.into_iter()
                .map(|expr| eval(table, expr))
                .map(|res| match res {
                    Ok(expr) => expr,
                    Err(e) => {
                        if failure.is_none() {
                            failure = Some(e);
                        }

                        Expression::Nil
                    }
                })
                .collect();

            match failure {
                Some(e) => Err(
                    LSHRuntimeErrorStack::create_stream("u", (0,0), e)
                ),
                None => Ok(Expression::DataList(results))
            }
        },

        Expression::Call(call) => {
            match call {
                Call::FunctionCall(symbol, args) => {
                    let func = eval(table,Expression::Symbol(symbol));
                    let args = eval(table, Expression::DataList(args));

                    if let Err(e) = args {
                        return Err(
                            LSHRuntimeErrorStack::create_stream("u", (0,0), e)
                        );
                    }

                    let args = args.unwrap().into();
                    
                    match func {
                        Err(e) => Err(
                            LSHRuntimeErrorStack::create_stream("u", (0,0), e)
                        ),
                        Ok(expr) => match expr {
                            Expression::Function(func) => func.exec(table, args),
                            _ => Err(LSHRuntimeErrorStack
                                ::create_source(
                                    "u", 
                                    (0,0), 
                                    LSHRuntimeErrorKind::NotCallable(&expr))
                                .append_note(String::from("A macro was expected"))
                            )
                        }
                    }

                },
                Call::MacroCall(symbol, args) => {
                    let mac = eval(table, Expression::Symbol(symbol));

                    match mac {
                        Err(e) => Err(
                            LSHRuntimeErrorStack::create_stream("u", (0,0), e)
                        ),
                        Ok(expr) => match expr {
                            Expression::Macro(mac) => mac.exec(table, args),
                            _ => Err(LSHRuntimeErrorStack
                                ::create_source(
                                    "u", 
                                    (0,0),
                                    LSHRuntimeErrorKind::NotCallable(&expr))
                                .append_note(String::from("A macro was expected"))
                            )
                        },
                    }
                },
            }
        },
        Expression::Symbol(symbol) => {
            match table.get(&symbol) {
                Some(value) => Ok(value),
                None => Err(LSHRuntimeErrorStack
                    ::create_source(
                        "u", 
                        (0,0), 
                        LSHRuntimeErrorKind::UndefinedIdentifier(&symbol)
                    )
                ),
            }
        },
        expr => Ok(expr)
    }
}