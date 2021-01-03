use std::{
    convert::TryFrom,
    iter::{Iterator, Peekable},
};

use logos::Logos;

use crate::ast::*;
use crate::token::Token;

pub struct Parser;

impl Parser {
    pub fn parse_program(source: &str) -> Result<Program, ParseError> {
        let tokens: Vec<Token> = Token::lexer(&source).collect();
        Program::parse(&mut tokens.into_iter().peekable())
    }

    /// If the next token is expected it is consumed. Otherwise, it is not consumed.
    fn accept<I>(tokens: &mut Peekable<I>, expected: Token) -> bool
    where
        I: Iterator<Item = Token>,
    {
        if tokens.peek() == Some(&expected) {
            tokens.next();
            true
        } else {
            false
        }
    }

    /// Consumes the next token. If the token is unexpected, then an error is produced.
    fn expect(tokens: &mut dyn Iterator<Item = Token>, expected: Token) -> Result<(), ParseError> {
        match tokens.next() {
            Some(token) => {
                if expected == token {
                    Ok(())
                } else {
                    Err(ParseError::unexpected(expected, token))
                }
            }
            None => Err(ParseError::end_of_stream()),
        }
    }
}

trait Parse {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        Self: Sized,
        I: Iterator<Item = Token>;
}

// TODO: Add span for error location.
#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    EndOfTokenStream,
    UnexpectedToken { expected: Token, found: Token },
    InvalidToken { found: Token },
}

impl ParseError {
    fn unexpected(expected: Token, found: Token) -> ParseError {
        ParseError {
            kind: ParseErrorKind::UnexpectedToken { expected, found },
        }
    }

    fn invalid(found: Token) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidToken { found },
        }
    }

    fn end_of_stream() -> ParseError {
        ParseError {
            kind: ParseErrorKind::EndOfTokenStream,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::EndOfTokenStream {} => {
                write!(f, "Expected token but reached end of token stream.")
            }
            ParseErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "Expected token {:?}, found {:?}.", expected, found)
            }
            ParseErrorKind::InvalidToken { found } => {
                write!(f, "Found invalid token {:?}.", found)
            }
        }
    }
}

/*** IMPLEMENTATIONS ***/

impl Parse for Program {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        let mut functions = vec![];
        while tokens.peek().is_some() {
            let function_declaration = FunctionDeclaration::parse(tokens)?;
            functions.push(function_declaration);
        }
        Ok(Program { functions })
    }
}

impl Parse for FunctionDeclaration {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        Parser::expect(tokens, Token::Fn)?;
        let name = match tokens.next().ok_or_else(ParseError::end_of_stream)? {
            Token::Identifier(name) => name,
            token => {
                return Err(ParseError::unexpected(
                    Token::Identifier("_".to_string()),
                    token,
                ))
            }
        };
        Parser::expect(tokens, Token::LeftParen)?;
        // TODO: Parameters.
        Parser::expect(tokens, Token::RightParen)?;

        let return_type = if Parser::accept(tokens, Token::Arrow) {
            match tokens.next().ok_or_else(ParseError::end_of_stream)? {
                Token::Identifier(name) => name,
                token => {
                    return Err(ParseError::unexpected(
                        Token::Identifier("_".to_string()),
                        token,
                    ))
                }
            }
        } else {
            "void".to_string()
        };

        let block = BlockExpression::parse(tokens)?;
        Ok(FunctionDeclaration {
            name,
            return_type,
            block,
        })
    }
}

impl Parse for Statement {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        if Parser::accept(tokens, Token::Let) {
            let name = match tokens.next().ok_or_else(ParseError::end_of_stream)? {
                Token::Identifier(name) => name,
                token => {
                    return Err(ParseError::unexpected(
                        Token::Identifier("_".to_string()),
                        token,
                    ))
                }
            };
            Parser::expect(tokens, Token::Colon)?;
            Parser::expect(tokens, Token::Identifier("i64".to_string()))?;
            Parser::expect(tokens, Token::Equal)?;
            let expression = Expression::parse(tokens)?;
            Parser::expect(tokens, Token::Semicolon)?;
            Ok(Statement::Assignment(name, expression))
        } else {
            let expression = Expression::parse(tokens)?;
            if Parser::accept(tokens, Token::Semicolon) {
                Ok(Statement::ExpressionStatement(expression))
            } else {
                Ok(Statement::Expression(expression))
            }
        }
    }
}

impl Parse for Expression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        fn parse_factor<I>(tokens: &mut Peekable<I>) -> Result<Expression, ParseError>
        where
            I: Iterator<Item = Token>,
        {
            if tokens.peek() == Some(&Token::LeftCurly) {
                return Ok(Expression::BlockExpression(BlockExpression::parse(tokens)?));
            }

            match tokens.next().ok_or_else(ParseError::end_of_stream)? {
                Token::LeftParen => {
                    let expression = Expression::parse(tokens)?;
                    Parser::expect(tokens, Token::RightParen)?;
                    Ok(expression)
                }
                Token::Minus => {
                    let operator = Operator::try_from(Token::Minus).unwrap();
                    let factor = parse_factor(tokens)?;
                    Ok(Expression::Unary(operator, Box::new(factor)))
                }
                Token::Number(value) => Ok(Expression::IntLiteral(value)),
                Token::Identifier(name) => Ok(Expression::Variable(name)),
                token => Err(ParseError::invalid(token)),
            }
        }

        fn parse_term<I>(tokens: &mut Peekable<I>) -> Result<Expression, ParseError>
        where
            I: Iterator<Item = Token>,
        {
            let mut expression = parse_factor(tokens)?;
            // TODO: Add division.
            while let Some(token) = tokens.next_if_eq(&Token::Multiply) {
                let operator = Operator::try_from(token).unwrap();
                let next_factor = parse_factor(tokens)?;
                expression =
                    Expression::Binary(operator, Box::new(expression), Box::new(next_factor));
            }
            Ok(expression)
        }

        fn parse_expression<I>(tokens: &mut Peekable<I>) -> Result<Expression, ParseError>
        where
            I: Iterator<Item = Token>,
        {
            let mut expression = parse_term(tokens)?;
            while let Some(token) =
                tokens.next_if(|token| matches!(token, Token::Plus | Token::Minus))
            {
                let operator = Operator::try_from(token).unwrap();
                let next_term = parse_term(tokens)?;
                expression =
                    Expression::Binary(operator, Box::new(expression), Box::new(next_term));
            }
            Ok(expression)
        }

        parse_expression(tokens)
    }
}

impl Parse for BlockExpression {
    fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        Parser::expect(tokens, Token::LeftCurly)?;
        let mut statements = vec![];
        // TODO: This does not terminate if already at end of iterator.
        while tokens.peek() != Some(&Token::RightCurly) {
            let statement = Statement::parse(tokens)?;
            statements.push(statement);
        }
        Parser::expect(tokens, Token::RightCurly)?;
        Ok(BlockExpression { statements })
    }
}
