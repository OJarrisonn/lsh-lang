pub mod eval;
pub mod parse;
pub mod error;

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::eval::symbol_table::SymbolTable;

    use super::*;

    #[test]
    fn parse_file() {
        let source = fs::read_to_string("./tests/printf_macro.lsh").unwrap();
        
        println!("{:#?}", parse::parse(&source));
    }

    #[test]
    fn sum() {
        let source = fs::read_to_string("./tests/sum.lsh").unwrap();
        let parsed = parse::parse(&source);
        let mut table = SymbolTable::new();
        table.init_native_functions();

        let result = eval::eval(&mut table, parsed.unwrap());

        dbg!(result);
    }
}