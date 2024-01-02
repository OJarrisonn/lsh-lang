pub mod eval;
pub mod parse;
pub mod error;
pub mod stl;

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
        table.load_stl_math();

        let result = eval::eval(&mut table, parsed.unwrap());

        dbg!(result);
    }

    #[test]
    fn math() {
        let source = fs::read_to_string("./tests/math.lsh").unwrap();
        let parsed = parse::parse(&source);
        let mut table = SymbolTable::new();
        table.load_stl_math();

        let result = eval::eval(&mut table, parsed.unwrap());

        dbg!(result);
    }


    #[test]
    fn def() {
        let source = fs::read_to_string("./tests/def.lsh").unwrap();
        let parsed = parse::parse(&source);
        let mut table = SymbolTable::new();
        table.load_stl();

        let result = eval::eval(&mut table, parsed.unwrap());

        dbg!(result);
    }

    #[test]
    fn print() {
        let source = fs::read_to_string("./tests/print.lsh").unwrap();
        let parsed = parse::parse(&source);
        let mut table = SymbolTable::new();
        table.load_stl();

        let result = eval::eval(&mut table, parsed.unwrap());

        dbg!(result);
    }
}