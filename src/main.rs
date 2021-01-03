#![feature(peekable_next_if)]

use std::io::{self, Read};

use inkwell::context::Context;
use logos::Logos;

mod ast;
mod codegen;
mod parse;
mod token;
mod visit;

use crate::ast::Program;
use crate::parse::Parse;
use crate::token::Token;
use crate::visit::Visitor;

fn main() {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source).unwrap();

    let tokens: Vec<Token> = Token::lexer(&source).collect();
    let program = Program::parse(&mut tokens.into_iter().peekable());
    println!("{:#?}", program);

    let context = Context::create();
    let mut code_generator = codegen::CodeGenerator::new(&context);
    code_generator.visit_program(&program);
    code_generator.finish();
}
