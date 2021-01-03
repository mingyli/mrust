use std::convert::TryFrom;

use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    // TODO: pub parameters: Vec<Parameter>,
    pub return_type: String, // TODO: Raise to a type.
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Assignment(String, Expression),
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(u64),
    Variable(String),
    Unary(Operator, Box<Expression>),
    Binary(Operator, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Times,
}

impl TryFrom<Token> for Operator {
    type Error = &'static str;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Operator::Plus),
            Token::Minus => Ok(Operator::Minus),
            Token::Multiply => Ok(Operator::Times),
            _ => Err("idk bad"),
        }
    }
}
