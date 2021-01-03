use std::iter::{Iterator, Peekable};

use crate::token::Token;

pub trait Parse {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
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
pub enum Expression {
    IntLiteral(u64),
    Unary(UnaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
}

impl Parse for Program {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
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

impl Parse for FunctionDeclaration {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
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

impl Parse for Statement {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
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

impl Parse for Expression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        match tokens.peek().unwrap().clone() {
            Token::Number(value) => {
                assert_eq!(tokens.next(), Some(Token::Number(value)));
                Expression::IntLiteral(value)
            }
            Token::Minus => Expression::Unary(
                UnaryOperator::parse(tokens),
                Box::new(Expression::parse(tokens)),
            ),
            _ => unreachable!(),
        }
    }
}

impl Parse for UnaryOperator {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        assert_eq!(tokens.next(), Some(Token::Minus));
        UnaryOperator::Negation
    }
}
