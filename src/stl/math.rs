use std::iter::zip;

use crate::{parse::expression::{Expression, Symbol}, error::{LSHRuntimeErrorStack, LSHRuntimeErrorKind}, eval::symbol_table::SymbolTable};

fn generic_binary_function<'a>(
    app: impl Fn(&Expression, &Expression) -> Result<Expression, LSHRuntimeErrorStack<'a>>, 
    a: &Expression, 
    b: &Expression
) -> Result<Expression, LSHRuntimeErrorStack<'a>> {
    if let Expression::DataList(bl) = b { // B is a data-list
        if let Expression::DataList(al) = a { // A is also a data-list, we'll apply the function item-wise
            if al.len() != bl.len() { // The inputs are list of different lengths
                Err(LSHRuntimeErrorStack
                    ::create_source(
                        "", 
                        (0,0),
                        LSHRuntimeErrorKind::IncompatibleArgument(
                            b, 
                            format!("Argument for `a` and `b` must be same length. |a| = {}, |b| = {}", 
                            al.len(), bl.len()))) 
                    )
            } else { // The inputs are the same size, let's apply it item by item
                zip(al, bl)
                    .map(|(a, b)| app(a, b))
                    .reduce(|res, acc| {
                        if let Err(e) = acc {
                            Err(e)
                        } else if let Err(e) = res {
                            Err(e)
                        } else {
                            let mut acc: Vec<Expression> = acc.unwrap().into();
                            acc.push(res.unwrap()); 
                            Ok(Expression::List(acc))
                        }
                    }).unwrap_or(Ok(Expression::Nil))
            }
        } else { // A is a scalar value
            bl.into_iter()
                .map(|b| app(a, b)) // Apply it on every item
                .reduce(|res, acc| { // Collect the result
                    if let Err(e) = acc { // Previous errors are propagated
                        Err(e)
                    } else if let Err(e) = res {
                        Err(e)
                    } else { // No previous or actual error, append the current result to the previous
                        let mut acc: Vec<Expression> = acc.unwrap().into();
                        acc.push(res.unwrap()); 
                        Ok(Expression::List(acc))
                    }
                }).unwrap_or(Ok(Expression::Nil)) // Return nil if both the lists are empty
        }
    } else { // B and A must be scalars
        if let Expression::DataList(_) = a { // A can't be a list
            Err(LSHRuntimeErrorStack
                ::create_source(
                    "", 
                    (0,0),
                    LSHRuntimeErrorKind::IncompatibleArgument(a, format!("As the argument for `b` isn't a data-list, neither `a` can be")) 
                )
            )
        } else {
            app(a, b) // a and b are scalars
        }
    }
}

fn generic_math_binary_application<'a>(app: impl Fn(f64, f64) -> f64) -> impl Fn(&Expression, &Expression) -> Result<Expression, LSHRuntimeErrorStack<'a>> {
    move |a, b| match (a, b) {
        (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Integer(app(*a as f64, *b as f64) as i64)),
        (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(app(*a, *b as f64))),
        (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(app(*a as f64, *b))),
        (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(app(*a, *b))),
        _ => Err(LSHRuntimeErrorStack::
            create_source(
                "", 
                (0,0),
                match (a, b) {
                    (Expression::Integer(_), _) | (Expression::Float(_), _) => LSHRuntimeErrorKind
                        ::IncompatibleArgument(b, format!("Argument for `b` must be an int or a float, but it's {}", b.type_name())),
                    (_, Expression::Integer(_)) | (_, Expression::Float(_)) => LSHRuntimeErrorKind
                        ::IncompatibleArgument(a, format!("Argument for `a` must be an int or a float, but it's {}", a.type_name())),
                    _ => LSHRuntimeErrorKind::Generic(format!(""))
                } 
            ))
    }

}


pub fn bin_add<'a>(st: &mut SymbolTable, args: Vec<&Expression>) -> Result<Expression, LSHRuntimeErrorStack<'a>> {
    if args.len() != 2 {
        Err(LSHRuntimeErrorStack
            ::create_source(
                "", 
                (0,0),
                LSHRuntimeErrorKind::TooManyArguments(2, args.len()) 
            )
        )
    } else {
        let (a, b) = (&args[0], &args[1]);

        generic_binary_function(generic_math_binary_application(|a, b| a+b), a, b)
    }
}

pub fn bin_sub(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHRuntimeErrorStack> {
    if args.len() != 2 {
        Err(LSHRuntimeErrorStack
            ::create_source(
                "", 
                (0,0),
                LSHRuntimeErrorKind::TooManyArguments(2, args.len()) 
            )
        )
    } else {
        let (a, b) = (&args[0], &args[1]);

        generic_binary_function(generic_math_binary_application(|a, b| a-b), a, b)
    }
}

pub fn bin_mul(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHRuntimeErrorStack> {
    if args.len() != 2 {
        Err(LSHRuntimeErrorStack
            ::create_source(
                "", 
                (0,0),
                LSHRuntimeErrorKind::TooManyArguments(2, args.len()) 
            )
        )
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Integer(a * b)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a * *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 * *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a * b)),
            _ => Err(LSHRuntimeErrorStack::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}
pub fn bin_div(_: &mut SymbolTable, args: Vec<Expression>) -> Result<Expression, LSHRuntimeErrorStack> {
    if args.len() != 2 {
        Err(LSHRuntimeErrorStack
            ::create_source(
                "", 
                (0,0),
                LSHRuntimeErrorKind::TooManyArguments(2, args.len()) 
            )
        )
    } else {
        let (a, b) = (&args[0], &args[1]);

        match (a, b) {
            (Expression::Integer(a), Expression::Integer(b)) => Ok(Expression::Float(*a as f64 / *b as f64)),
            (Expression::Float(a), Expression::Integer(b)) => Ok(Expression::Float(a / *b as f64)),
            (Expression::Integer(a), Expression::Float(b)) => Ok(Expression::Float(*a as f64 / *b)),
            (Expression::Float(a), Expression::Float(b)) => Ok(Expression::Float(a / b)),
            _ => Err(LSHRuntimeErrorStack::unexpected_arg_type(Symbol::Identifier("a, b".to_string())))
        }
    }
}