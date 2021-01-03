use std::iter::{Iterator, Peekable};

use crate::token::Token;

pub trait Parse<T> {
    fn parse<I>(tokens: &mut Peekable<I>) -> T
    where
        I: Iterator<Item = Token>;
}

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
pub struct Expression(pub i64);

impl Parse<Program> for Program {
    fn parse<I>(tokens: &mut Peekable<I>) -> Program
    where
        I: Iterator<Item = Token>,
    {
        let mut functions = vec![];
        while tokens.peek().is_some() {
            let function_declaration = FunctionDeclaration::parse(tokens);
            functions.push(function_declaration);
        }
        Program { functions }
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
        // TODO: Parameters.
        assert_eq!(tokens.next(), Some(Token::RightParen));

        let return_type = if tokens.peek() == Some(&Token::Arrow) {
            assert_eq!(tokens.next(), Some(Token::Arrow));
            match tokens.next().unwrap() {
                Token::Identifier(name) => name,
                _ => unreachable!(),
            }
        } else {
            "void".to_string()
        };

        assert_eq!(tokens.next(), Some(Token::LeftCurly));
        let mut statements = vec![];

        // TODO: This does not terminate if at end of iterator.
        while tokens.peek() != Some(&Token::RightCurly) {
            let statement = Statement::parse(tokens);
            statements.push(statement);
        }
        assert_eq!(tokens.next(), Some(Token::RightCurly));
        FunctionDeclaration {
            name,
            return_type,
            statements,
        }
    }
}

impl Parse<Statement> for Statement {
    fn parse<I>(tokens: &mut Peekable<I>) -> Statement
    where
        I: Iterator<Item = Token>,
    {
        if tokens.peek() == Some(&Token::Let) {
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
        } else {
            let expression = Expression::parse(tokens);
            Statement::Expression(expression)
        }
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
