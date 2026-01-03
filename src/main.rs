use std::fs;

use lexi_lang::lexer::lexer::Lexer;
use lexi_lang::lexer::tokenlist::*;
use lexi_lang::errorhandler::errorhandler::*;
use lexi_lang::errorhandler::errorlist::*;

fn main() {
    let contents: String = fs::read_to_string("progs/demo.lexi")
        .expect("Failed to read file!");
    let mut lexer = Lexer::init();
    let mut token_list: Vec<Token> = Vec::new();
    match lexer.tokenize(contents) {
        Ok(t) => token_list = t,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        },
    }
    println!("{:?}", token_list);
}