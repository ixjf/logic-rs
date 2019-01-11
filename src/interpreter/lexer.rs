use super::source_reader::*;

pub enum Token {
    GrouperOpening,
    GrouperClosing,
    SimpleStatement(char, Option<u32>),
    ConjunctionConnective,
    NegationConnective,
    DisjunctionConnective,
    ConditionalConnective,
    StatementSetOpening,
    StatementSeparator,
    StatementSetClosing,
    ArgumentConclusionIndicator
}

pub enum ErrorKind {
    InvalidToken
}

pub struct Error {
    pub kind: ErrorKind,
    pub line: u32,
    pub col: u32
}

enum InternalErrorKind {
    InvalidSubscriptNumber,
    NoAction
}
pub struct Lexer {
    sr: SourceReader
}

impl Lexer {
    pub fn new(sr: SourceReader) -> Self {
        Lexer { sr }
    }

    pub fn validate(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(curr_char) = self.sr.next() {
            // INCOMPLETE: ONLY IMPLEMENTS PL GRAMMAR
            // Applies rule 1 of interpreter-specific grammar
            // That is, whitespaces/return are allowed & skipped
            // between any expression
            if curr_char == '(' {
                tokens.push(Token::GrouperOpening);
            }
            else if curr_char == ')' {
                tokens.push(Token::GrouperClosing);
            }
            else if curr_char.is_ascii_uppercase() {
                match self.try_read_subscript_number() {
                    Ok(subscript_number) => tokens.push(Token::SimpleStatement(curr_char, Some(subscript_number))),
                    Err(e) => match e {
                        InternalErrorKind::InvalidSubscriptNumber => return Err(self.new_error(ErrorKind::InvalidToken)),
                        _ => tokens.push(Token::SimpleStatement(curr_char, None))
                    }
                }
            }
            else if curr_char == '&' {
                tokens.push(Token::ConjunctionConnective);
            }
            else if curr_char == '~' {
                tokens.push(Token::NegationConnective);
            }
            else if curr_char == '\u{2228}' {
                tokens.push(Token::DisjunctionConnective);
            }
            else if curr_char == '\u{2283}' {
                tokens.push(Token::ConditionalConnective);
            }
            else if curr_char == '{' {
                tokens.push(Token::StatementSetOpening);
            }
            else if curr_char == ',' {
                tokens.push(Token::StatementSeparator);
            }
            else if curr_char == '}' {
                tokens.push(Token::StatementSetClosing);
            }
            else if curr_char == '.' && self.try_read_argument_conclusion_indicator() {
                tokens.push(Token::ArgumentConclusionIndicator);
            }
            else if curr_char == '\r' || curr_char == '\n' || curr_char == ' ' || curr_char == '\t' {
                continue;
            }
            else {
                return Err(self.new_error(ErrorKind::InvalidToken));
            }
        }

        Ok(tokens)
    }

    fn new_error(&self, kind: ErrorKind) -> Error {
        Error { kind, line: self.sr.current_line(), col: self.sr.current_column() }
    }

    fn try_read_subscript_number(&mut self) -> Result<u32, InternalErrorKind> {
        let mut subscript_number = String::new();

        while let Some(c) = self.sr.peek_forward() {
            match c {
                '\u{2080}' => { subscript_number.push('0'); self.sr.next(); },
                '\u{2081}' => { subscript_number.push('1'); self.sr.next(); },
                '\u{2082}' => { subscript_number.push('2'); self.sr.next(); },
                '\u{2083}' => { subscript_number.push('3'); self.sr.next(); },
                '\u{2084}' => { subscript_number.push('4'); self.sr.next(); },
                '\u{2085}' => { subscript_number.push('5'); self.sr.next(); },
                '\u{2086}' => { subscript_number.push('6'); self.sr.next(); },
                '\u{2087}' => { subscript_number.push('7'); self.sr.next(); },
                '\u{2088}' => { subscript_number.push('8'); self.sr.next(); },
                '\u{2089}' => { subscript_number.push('9'); self.sr.next(); },
                _ => break
            }
        }

        if subscript_number.is_empty() {
            return Err(InternalErrorKind::NoAction);
        }

        if subscript_number.starts_with('0') {
            return Err(InternalErrorKind::InvalidSubscriptNumber);
        }

        Ok(subscript_number.parse::<u32>().unwrap())
    }

    fn try_read_argument_conclusion_indicator(&mut self) -> bool { // or whatever is left of it
        match self.sr.multipeek_forward(2) {
            Some(ref s) if s == ":." => {
                self.sr.skip(2);
                true
            },
            _ => false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn understands_simple_statements() {
        let mut l = Lexer::new(SourceReader::new("A"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 1);

                for token in token_set {
                    match token {
                        Token::SimpleStatement(name, subscript) => {
                            assert_eq!(name, 'A');
                            assert_eq!(subscript, None);
                        },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_simple_statements_with_subscript() {
        let mut l = Lexer::new(SourceReader::new("A\u{2081}"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 1);

                for token in token_set {
                    match token {
                        Token::SimpleStatement(name, subscript) => {
                            assert_eq!(name, 'A');
                            assert_eq!(subscript, Some(1));
                        },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_simple_statements_with_long_subscript() {
        let mut l = Lexer::new(SourceReader::new("A\u{2081}\u{2083}\u{2086}\u{2088}\u{2080}"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 1);

                for token in token_set {
                    match token {
                        Token::SimpleStatement(name, subscript) => {
                            assert_eq!(name, 'A');
                            assert_eq!(subscript, Some(13680));
                        },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn doesnt_understand_simple_statements_with_leading_zero_in_subscript() {
        let mut l = Lexer::new(SourceReader::new("A\u{2080}\u{2083}\u{2086}\u{2088}\u{2080}"));

        match l.validate() {
            Ok(_) => assert!(false),
            _ => { }
        }
    }

    #[test]
    fn understands_groupers() {
        let mut l = Lexer::new(SourceReader::new("()"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 2);

                for token in token_set {
                    match token {
                        Token::GrouperOpening => { },
                        Token::GrouperClosing => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn ignores_spaces_between_tokens() {
        let mut l = Lexer::new(SourceReader::new(" (  &   A  )"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 4);
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_argument_conclusion_indicator() {
        let mut l = Lexer::new(SourceReader::new("A .:. B"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 3);

                for token in token_set {
                    match token {
                        Token::SimpleStatement(_, _) => { },
                        Token::ArgumentConclusionIndicator => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn doesnt_understand_dot_that_isnt_argument_conclusion_indicator() {
        let mut l = Lexer::new(SourceReader::new("A. B"));

        match l.validate() {
            Ok(_) => assert!(false),
            _ => { }
        }
    }

    #[test]
    fn understands_statement_separator() {
        let mut l = Lexer::new(SourceReader::new("A,B"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 3);

                for token in token_set {
                    match token {
                        Token::SimpleStatement(_, _) => { },
                        Token::StatementSeparator => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_logical_connectives() {
        let mut l = Lexer::new(SourceReader::new("&~\u{2228}\u{2283}"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 4);

                for token in token_set {
                    match token {
                        Token::ConjunctionConnective => { },
                        Token::NegationConnective => { },
                        Token::DisjunctionConnective => { },
                        Token::ConditionalConnective => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_statement_sets() {
        let mut l = Lexer::new(SourceReader::new("{{ } { }}}"));

        match l.validate() {
            Ok(token_set) => {
                assert_eq!(token_set.len(), 7);

                for token in token_set {
                    match token {
                        Token::StatementSetOpening => { },
                        Token::StatementSetClosing => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }
}