use std::{
    convert::TryFrom,
    iter::{Iterator, Peekable},
};

use crate::ast::*;
use crate::token::Token;

pub trait Parse {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>;
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

        let return_type = if tokens.next_if_eq(&Token::Arrow).is_some() {
            match tokens.next().unwrap() {
                Token::Identifier(name) => name,
                _ => unreachable!(),
            }
        } else {
            "void".to_string()
        };

        let block = BlockExpression::parse(tokens);
        FunctionDeclaration {
            name,
            return_type,
            block,
        }
    }
}

impl Parse for Statement {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        if tokens.next_if_eq(&Token::Let).is_some() {
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
            if tokens.next_if_eq(&Token::Semicolon).is_some() {
                Statement::ExpressionStatement(expression)
            } else {
                Statement::Expression(expression)
            }
        }
    }
}

impl Parse for Expression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        fn parse_factor<I>(tokens: &mut Peekable<I>) -> Expression
        where
            I: Iterator<Item = Token>,
        {
            if tokens.peek() == Some(&Token::LeftCurly) {
                return Expression::BlockExpression(BlockExpression::parse(tokens));
            }

            match tokens.next().unwrap() {
                Token::LeftParen => {
                    let expression = Expression::parse(tokens);
                    assert_eq!(tokens.next(), Some(Token::RightParen));
                    expression
                }
                Token::Minus => {
                    let operator = Operator::try_from(Token::Minus).unwrap();
                    let factor = parse_factor(tokens);
                    Expression::Unary(operator, Box::new(factor))
                }
                Token::Number(value) => Expression::IntLiteral(value),
                Token::Identifier(name) => Expression::Variable(name),
                _ => unreachable!(),
            }
        }

        fn parse_term<I>(tokens: &mut Peekable<I>) -> Expression
        where
            I: Iterator<Item = Token>,
        {
            let mut expression = parse_factor(tokens);
            while let Some(token) = tokens.next_if_eq(&Token::Multiply) {
                let operator = Operator::try_from(token).unwrap();
                let next_factor = parse_factor(tokens);
                expression =
                    Expression::Binary(operator, Box::new(expression), Box::new(next_factor));
            }
            expression
        }

        fn parse_expression<I>(tokens: &mut Peekable<I>) -> Expression
        where
            I: Iterator<Item = Token>,
        {
            let mut expression = parse_term(tokens);
            while let Some(token) =
                tokens.next_if(|token| matches!(token, Token::Plus | Token::Minus))
            {
                let operator = Operator::try_from(token).unwrap();
                let next_term = parse_term(tokens);
                expression =
                    Expression::Binary(operator, Box::new(expression), Box::new(next_term));
            }
            expression
        }

        parse_expression(tokens)
    }
}

impl Parse for BlockExpression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        assert_eq!(tokens.next(), Some(Token::LeftCurly));
        let mut statements = vec![];
        // TODO: This does not terminate if already at end of iterator.
        while tokens.peek() != Some(&Token::RightCurly) {
            let statement = Statement::parse(tokens);
            statements.push(statement);
        }
        assert_eq!(tokens.next(), Some(Token::RightCurly));
        BlockExpression { statements }
    }
}
