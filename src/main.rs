use inkwell::context::Context;
use logos::Logos;

mod ast;
mod codegen;
mod token;
mod visit;

use crate::ast::{Parse, Program};
use crate::token::Token;
use crate::visit::Visitor;

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
    fn foo() -> i64 {
        let x: i64 = 7;
        4
    }

    fn main() {
        let x: i64 = 5;
        let y: i64 = 6;
    }
    "#;
    let tokens: Vec<Token> = Token::lexer(source).collect();
    let program = Program::parse(&mut tokens.into_iter().peekable());
    println!("{:#?}", program);

    let context = Context::create();
    let mut code_generator = codegen::CodeGenerator::new(&context);
    code_generator.visit_program(&program);
    code_generator.finish();
}
