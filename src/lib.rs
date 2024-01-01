pub mod eval;
pub mod parse;
pub mod symbol_table;
pub mod error;

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn parse_file() {
        let source = fs::read_to_string("./tests/.lshrc").unwrap();
        
        println!("{:?}", parse::parse(&source));
    }
}