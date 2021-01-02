use logos::Logos;

mod ast;
mod token;

use crate::ast::{Parse, Program};
use crate::token::Token;

fn main() {
    let _source = r#"
    fn foo(a: i64, b: i64) -> i64 {
        a + b
    }

    fn main() {
        let x: i64 = 5;
        let y: i64 = {
            let z: i64 = 3;
            x + z
        };
        let z: i64 = foo(x, y);
    }
    "#;
    let source = r#"
    fn main() {
        let x: i64 = 5;
    }
    "#;
    let tokens: Vec<Token> = Token::lexer(source).collect();
    let program = Program::parse(&mut tokens.into_iter().peekable());
    println!("{:#?}", program);
}
