use std::collections::HashMap;

use crate::{parse::expression::{Symbol, Expression, Function, Macro}, stl::{math, def, io, logic}};

pub struct SymbolTable(HashMap<Symbol, Expression>);


impl SymbolTable {
    pub fn new() -> Self {
        let table = HashMap::new();

        Self(table)
    }

    pub fn load_stl(&mut self) {
        self.load_stl_math();
        self.load_stl_def();
        self.load_stl_io();
        self.load_stl_logic();
    }

    pub fn load_stl_math(&mut self) {
        self.set(Symbol::Identifier("+".to_string()), Expression::Function(Function::Native(math::bin_add)));
        self.set(Symbol::Identifier("-".to_string()), Expression::Function(Function::Native(math::bin_sub)));
        self.set(Symbol::Identifier("*".to_string()), Expression::Function(Function::Native(math::bin_mul)));
        self.set(Symbol::Identifier("/".to_string()), Expression::Function(Function::Native(math::bin_div)));

        self.set(Symbol::Identifier("pi".to_string()), Expression::Float(3.14159268))
    }

    pub fn load_stl_def(&mut self) {
        self.set(Symbol::MacroIdentifier("@def".to_string()), Expression::Macro(Macro::Native(def::def)))
    }

    pub fn load_stl_io(&mut self) {
        self.set(Symbol::Identifier("print".to_string()), Expression::Function(Function::Native(io::print)))
    }

    pub fn load_stl_logic(&mut self) {
        self.set(Symbol::Identifier("is-true?".to_string()), Expression::Function(Function::Native(logic::is_true)));
        self.set(Symbol::Identifier("is-false?".to_string()), Expression::Function(Function::Native(logic::is_false)));
        self.set(Symbol::Identifier("if".to_string()), Expression::Function(Function::Native(logic::tern_if)));
        self.set(Symbol::Identifier(">".to_string()), Expression::Function(Function::Native(logic::bin_gt)));
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