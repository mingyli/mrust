use std::iter::{Iterator, Peekable};

use crate::token::Token;

pub trait Parse<T> {
    fn parse<I>(tokens: &mut Peekable<I>) -> T
    where
        I: Iterator<Item = Token>;
}

#[derive(Debug)]
pub struct Program(FunctionDeclaration);

#[derive(Debug)]
struct FunctionDeclaration(String, Vec<Statement>);

#[derive(Debug)]
enum Statement {
    Assignment(String, Expression),
}

#[derive(Debug)]
struct Expression(i64);

impl Parse<Program> for Program {
    fn parse<I>(tokens: &mut Peekable<I>) -> Program
    where
        I: Iterator<Item = Token>,
    {
        let function_declaration = FunctionDeclaration::parse(tokens);
        Program(function_declaration)
    }
}

impl Parse<FunctionDeclaration> for FunctionDeclaration {
    fn parse<I>(tokens: &mut Peekable<I>) -> FunctionDeclaration
    where
        I: Iterator<Item = Token>,
    {
        assert_eq!(tokens.next(), Some(Token::Fn));
        let name = match tokens.next().unwrap() {
            Token::Identifier(name) => name,
            _ => unreachable!(),
        };
        assert_eq!(tokens.next(), Some(Token::LeftParen));
        assert_eq!(tokens.next(), Some(Token::RightParen));
        assert_eq!(tokens.next(), Some(Token::LeftCurly));
        let statement = Statement::parse(tokens);
        assert_eq!(tokens.next(), Some(Token::RightCurly));
        FunctionDeclaration(name, vec![statement])
    }
}

impl Parse<Statement> for Statement {
    fn parse<I>(tokens: &mut Peekable<I>) -> Statement
    where
        I: Iterator<Item = Token>,
    {
        assert_eq!(tokens.next(), Some(Token::Let));
        let name = match tokens.next().unwrap() {
            Token::Identifier(name) => name,
            _ => unreachable!(),
        };
        assert_eq!(tokens.next(), Some(Token::Colon));
        assert_eq!(tokens.next(), Some(Token::Identifier("i64".to_string())));
        assert_eq!(tokens.next(), Some(Token::Equal));
        let expression = Expression::parse(tokens);
        assert_eq!(tokens.next(), Some(Token::Semicolon));
        Statement::Assignment(name, expression)
    }
}

impl Parse<Expression> for Expression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Expression
    where
        I: Iterator<Item = Token>,
    {
        let value = match tokens.next().unwrap() {
            Token::Number(value) => value,
            _ => unreachable!(),
        };
        Expression(value)
    }
}
