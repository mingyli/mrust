use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftCurly,
    #[token("}")]
    RightCurly,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token("=")]
    Equal,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("->")]
    Arrow,
    #[regex(r"[a-zA-Z]\w*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Number(i64),

    #[error]
    #[regex(r"[ \t\n]+", logos::skip)]
    Error,
}
