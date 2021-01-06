#![feature(peekable_next_if)]

use std::io::{self, Read};

use inkwell::context::Context;

mod ast;
mod codegen;
mod parse;
mod token;
mod types;
mod visit;

use crate::parse::Parser;
use crate::visit::Visitor;

fn main() {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source).unwrap();
    let program = Parser::parse_program(&source).unwrap();

    let context = Context::create();
    let mut code_generator = codegen::CodeGenerator::new(&context);
    code_generator.visit_program(&program);
    code_generator.finish();
}
