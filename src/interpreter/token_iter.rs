use mystd::iter::streaming::*;
use super::token::*;

pub enum Error {
    UnexpectedTokenKind,
    EndOfStream
}

pub struct TokenIterator<'a> {
    iter: StreamingIterator<'a, Token>,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        TokenIterator { iter: StreamingIterator::new(tokens) }
    }

    pub fn try_read_token_of_kind(&mut self, kind: TokenKind) -> Result<(), Error> {
        match self.iter.next() {
            Some(ref token) => match token.kind() {
                kind => Ok(()),
                a @ _ => Err(Error::UnexpectedTokenKind)
            },
            None => Err(Error::EndOfStream)
        }
    }

    pub fn next(&mut self) -> Option<&'a Token> {
        self.iter.next()
    }

    pub fn prev(&mut self) -> Option<&'a Token> {
        self.iter.prev()
    }
}

impl<'a> Clone for TokenIterator<'a> {
    fn clone(&self) -> Self {
        TokenIterator { iter: self.iter.clone() }
    }
}