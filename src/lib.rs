use pest::Parser;
use pest_derive::Parser;

pub mod ast;

#[derive(Parser)]
#[grammar = "lsh.pest"]
pub struct LSHParser;

pub fn parse(source: String) {
    let a = LSHParser::parse(Rule::program, &source);
}