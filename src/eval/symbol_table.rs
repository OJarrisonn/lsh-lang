use std::collections::HashMap;

use crate::{parse::expression::{Symbol, Expression, self, Function}, error::LSHError};

use super::native_functions;

pub struct SymbolTable(HashMap<Symbol, Expression>);


impl SymbolTable {
    pub fn new() -> Self {
        let table = HashMap::new();

        Self(table)
    }

    pub fn init_native_functions(&mut self) {
        self.set(Symbol::Identifier("+".to_string()), Expression::Function(Function::Native(native_functions::bin_add)))
    }

    pub fn get(&self, symbol: &Symbol) -> Option<Expression> {
        match self.0.get(symbol) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, symbol: Symbol, expression: Expression) {
        self.0.insert(symbol, expression);
    }
}