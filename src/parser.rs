use crate::expr::{Expr, VarName, abs, app, var};
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    ParenOpen,
    ParenClose,
    Lambda,
    Dot,
    Eof,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(i) => write!(f, "{i}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Lambda => write!(f, "λ"),
            Token::Dot => write!(f, "."),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    ControlChar,
    UnmatchedParen,
    UnexpectedParen,
    UnexpectedToken(String),
    MissingParam,
    MissingBody,
    Eof,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::ControlChar => write!(f, "unexpected control character"),
            ParserError::UnmatchedParen => write!(f, "unmatched parentheses"),
            ParserError::UnexpectedParen => write!(f, "unexpected parentheses"),
            ParserError::UnexpectedToken(t) => write!(f, "unexpected token `{t}`"),
            ParserError::MissingParam => write!(f, "function is missing a parameter"),
            ParserError::MissingBody => write!(f, "function is missing a body"),
            ParserError::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            src: src.trim(),
            pos: 0,
        }
    }

    fn bump(&mut self, n: usize) {
        self.pos += n;
    }

    fn next(&mut self) -> Result<Token<'a>, ParserError> {
        let curr = &self.src[self.pos..];
        let fst = if let Some(c) = curr.chars().next() {
            c
        } else {
            return Ok(Token::Eof);
        };

        match fst {
            '\\' | 'λ' => {
                self.bump(fst.len_utf8());
                Ok(Token::Lambda)
            }
            '(' => {
                self.bump(fst.len_utf8());
                Ok(Token::ParenOpen)
            }
            ')' => {
                self.bump(fst.len_utf8());
                Ok(Token::ParenClose)
            }
            '.' => {
                self.bump(fst.len_utf8());
                Ok(Token::Dot)
            }
            s if s.is_whitespace() => {
                self.bump(s.len_utf8());
                self.next()
            }
            _ => {
                if let Some(spaces) = curr.find(|c: char| !c.is_whitespace()) {
                    self.bump(spaces);
                } else {
                    self.bump(curr.len());
                    return Ok(Token::Eof);
                }
                let rest = &self.src[self.pos..];
                if let Some(expected_ident_len) =
                    rest.find(|c: char| !(c.is_alphanumeric() || c == '_'))
                {
                    let ident = &rest[..expected_ident_len];
                    if ident.contains(char::is_control) {
                        return Err(ParserError::ControlChar);
                    }
                    self.bump(expected_ident_len);
                    return Ok(Token::Ident(ident));
                }
                let len = rest.len();
                self.bump(len);
                Ok(Token::Ident(rest))
            }
        }
    }

    fn peek_next(&self) -> Result<Token<'a>, ParserError> {
        self.clone().next()
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let next = self.peek_next()?;
            if next == Token::ParenClose || next == Token::Eof {
                break;
            }
            let rhs = match self.parse_primary() {
                Ok(expr) => expr,
                Err(ParserError::Eof) => return Ok(lhs),
                err => return err,
            };
            lhs = app!(lhs, rhs);
        }

        Ok(lhs)
    }

    fn parse_abs(&mut self) -> Result<Expr, ParserError> {
        let curr = self.next()?;
        let param = if let Token::Ident(x) = curr {
            x
        } else {
            return Err(ParserError::MissingParam);
        };
        let body = if let Token::Dot = self.next()? {
            self.parse_expr()?
        } else {
            return Err(ParserError::MissingBody);
        };
        Ok(abs!(param, body))
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let curr = self.next()?;
        match curr {
            Token::ParenOpen => {
                let expr = self.parse_expr()?;
                let next = self.next()?;
                if next != Token::ParenClose {
                    return Err(ParserError::UnmatchedParen);
                }
                Ok(expr)
            }
            Token::Lambda => self.parse_abs(),
            Token::Ident(name) => {
                assert!(!name.is_empty(), "Shouldn't happen");
                Ok(var!(name))
            }
            Token::Eof => Err(ParserError::Eof),
            Token::ParenClose => Err(ParserError::UnexpectedParen),
            t => Err(ParserError::UnexpectedToken(t.to_string())),
        }
    }
}
