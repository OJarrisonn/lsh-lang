use std::collections::HashMap;

use crate::parse::{Symbol, Expression};

pub struct SymbolTable(HashMap<Symbol, Expression>);


impl SymbolTable {
    fn init() -> Self {
        let mut st = HashMap::new();

        Self(st)
    }
}