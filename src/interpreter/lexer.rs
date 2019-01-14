use super::source_reader::*;
use super::token::*;

pub enum ErrorKind {
    InvalidToken
}

pub struct Error(ErrorKind, u32, u32);

pub struct Lexer<'a> {
    sr: &'a mut SourceReader<'a>
}

impl<'a> Lexer<'a> {
    pub fn new(sr: &'a mut SourceReader<'a>) -> Self {
        Lexer { sr }
    }

    pub fn validate(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(&curr_char) = self.sr.next() {
            // INCOMPLETE: ONLY IMPLEMENTS PL GRAMMAR
            // Applies rule 1 of interpreter-specific grammar
            //  That is, whitespaces/return are allowed & skipped
            //  between any expression
            // And rules 1-4 of PL
            if curr_char == '(' {
                tokens.push(self.new_token(TokenKind::GrouperOpening, TokenAssocData::GrouperOpening(())));
            }
            else if curr_char == ')' {
                tokens.push(self.new_token(TokenKind::GrouperClosing, TokenAssocData::GrouperClosing(())));
            }
            else if curr_char.is_ascii_uppercase() {
                match self.try_read_subscript_number() {
                    Ok(o) => tokens.push(self.new_token(TokenKind::SimpleStatement, TokenAssocData::SimpleStatement(curr_char, o))),
                    Err(e) => return Err(self.new_error(e))
                }
            }
            else if curr_char == '&' {
                tokens.push(self.new_token(TokenKind::ConjunctionConnective, TokenAssocData::ConjunctionConnective(())));
            }
            else if curr_char == '~' {
                tokens.push(self.new_token(TokenKind::NegationConnective, TokenAssocData::NegationConnective(())));
            }
            else if curr_char == '\u{2228}' {
                tokens.push(self.new_token(TokenKind::DisjunctionConnective, TokenAssocData::DisjunctionConnective(())));
            }
            else if curr_char == '\u{2283}' {
                tokens.push(self.new_token(TokenKind::ConditionalConnective, TokenAssocData::ConditionalConnective(())));
            }
            else if curr_char == '{' {
                tokens.push(self.new_token(TokenKind::StatementSetOpening, TokenAssocData::StatementSetOpening(())));
            }
            else if curr_char == ',' {
                tokens.push(self.new_token(TokenKind::StatementSeparator, TokenAssocData::StatementSeparator(())));
            }
            else if curr_char == '}' {
                tokens.push(self.new_token(TokenKind::StatementSetClosing, TokenAssocData::StatementSetClosing(())));
            }
            else if curr_char == '.' && self.try_read_argument_conclusion_indicator() {
                tokens.push(self.new_token(TokenKind::ArgumentConclusionIndicator, TokenAssocData::ArgumentConclusionIndicator(())));
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
        Error(kind, self.sr.current_line(), self.sr.current_column())
    }

    fn new_token(&self, kind: TokenKind, assoc_data: TokenAssocData) -> Token {
        Token::new(kind, assoc_data, self.sr.current_line(), self.sr.current_column())
    }

    fn try_read_subscript_number(&mut self) -> Result<Option<u32>, ErrorKind> {
        // Applies rule 6 of PL grammar
        let mut subscript_number = String::new();

        while let Some(&c) = self.sr.peek() {
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
            return Ok(None);
        }

        if subscript_number.starts_with('0') {
            return Err(ErrorKind::InvalidToken);
        }

        Ok(Some(subscript_number.parse::<u32>().unwrap()))
    }

    fn try_read_argument_conclusion_indicator(&mut self) -> bool { // or whatever is left of it
        // Applies rule 3 of interpreter-specific grammar
        match self.sr.peek_some(2) {
            Some(ref s) if s == ":." => { self.sr.skip(2); return true; },
            _ => return false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn understands_simple_statements() {
        let source = vec!['A'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 1);

                for token in tokens {
                    match token.assoc_data() {
                        TokenAssocData::SimpleStatement(name, subscript) => {
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
        let source = vec!['A', '\u{2081}'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 1);

                for token in tokens {
                    match token.assoc_data() {
                        TokenAssocData::SimpleStatement(name, subscript) => {
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
        let source = vec!['A', '\u{2081}', '\u{2083}', '\u{2086}', '\u{2088}', '\u{2080}'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 1);

                for token in tokens {
                    match token.assoc_data() {
                        TokenAssocData::SimpleStatement(name, subscript) => {
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
        let source = vec!['A', '\u{2080}', '\u{2083}', '\u{2086}', '\u{2088}', '\u{2080}'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(_) => assert!(false),
            _ => { }
        }
    }

    #[test]
    fn understands_groupers() {
        let source = vec!['(', ')'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 2);

                for token in tokens {
                    match token.kind() {
                        TokenKind::GrouperOpening => { },
                        TokenKind::GrouperClosing => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn ignores_spaces_between_tokens() {
        let source = vec![' ', '(', ' ', ' ', '&', ' ', ' ', 'A', ' ', ')'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 4);
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_argument_conclusion_indicator() {
        let source = vec!['A', ' ', '.', ':', '.', ' ', 'B'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 3);

                for token in tokens {
                    match token.kind() {
                        TokenKind::SimpleStatement => { },
                        TokenKind::ArgumentConclusionIndicator => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn doesnt_understand_dot_that_isnt_argument_conclusion_indicator() {
        let source = vec!['A', '.', ' ', 'B'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(_) => assert!(false),
            _ => { }
        }
    }

    #[test]
    fn understands_statement_separator() {
        let source = vec!['A', ',', 'B'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 3);

                for token in tokens {
                    match token.kind() {
                        TokenKind::SimpleStatement => { },
                        TokenKind::StatementSeparator => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_logical_connectives() {
        let source = vec!['&', '~', '\u{2228}', '\u{2283}'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 4);

                for token in tokens {
                    match token.kind() {
                        TokenKind::ConjunctionConnective => { },
                        TokenKind::NegationConnective => { },
                        TokenKind::DisjunctionConnective => { },
                        TokenKind::ConditionalConnective => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn understands_statement_sets() {
        let source = vec!['{', '{', ' ', '}', ' ', '{', ' ', '}', '}', '}'];
        let mut sr = SourceReader::new(&source);
        let mut l = Lexer::new(&mut sr);

        match l.validate() {
            Ok(tokens) => {
                assert_eq!(tokens.len(), 7);

                for token in tokens {
                    match token.kind() {
                        TokenKind::StatementSetOpening => { },
                        TokenKind::StatementSetClosing => { },
                        _ => assert!(false)
                    }
                }
            },
            Err(_) => assert!(false)
        }
    }
}