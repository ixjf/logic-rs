use super::token_iter::*;
use super::token::*;

pub struct Parser<'a> {
    token_iter: TokenIterator<'a>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { token_iter: TokenIterator::new(tokens) }
    }

    pub fn validate(&mut self) -> Result<(), Error> {
        println!("validate");

        // TODO!
        let mut iter = self.token_iter.clone();
        self.read_until(&mut iter, None)
    }

    fn read_until(&mut self, iter: &mut TokenIterator<'a>, until_kind: Option<TokenKind>) -> Result<(), Error> {
        println!("read_until");
        // We look into the ABNF rules here
        // We ask: is there a rule that begins with this token?
        // If there is, then try to parse it
        // If there isn't, then it's a syntax error, since that
        // token cannot appear alone and in any normal case
        // it would have been consumed by a try_x function

        let mut found_match = false;

        while let Some(token) = iter.clone().next() {
            match until_kind {
                Some(a) => match token.kind() {
                    a => {found_match = true; break;},
                    _ => {}
                },
                None => {}
            }

            let mut _iter = self.token_iter.clone();

            match token.kind() {
                TokenKind::GrouperOpening => {
                    // Groupers begin with this token
                    // Logical conjunctions begin with this token
                    // Logical disjunctions begin with this token
                    // Logical conditionals begin with this token
                    match self.try_read_grouper(&mut _iter).or_else(|_| 
                          self.try_read_logical_conjunction(&mut _iter)).or_else(|_| 
                          self.try_read_logical_disjunction(&mut _iter)).or_else(|_| 
                          self.try_read_logical_conditional(&mut _iter)) {
                        Ok(_) => continue,
                        Err(e)=> unimplemented!()
                    }
                },
                TokenKind::GrouperClosing => unimplemented!(),
                TokenKind::SimpleStatement => {
                    // Simple statements begin with this token
                    match self.try_read_simple_statement(&mut _iter) {
                        Ok(_) => continue,
                        Err(e)=> unimplemented!()
                    }
                },
                TokenKind::ConjunctionConnective => unimplemented!(),
                TokenKind::NegationConnective => {
                    match self.try_read_logical_negation(&mut _iter) {
                        Ok(_) => continue,
                        Err(e)=> unimplemented!()
                    }
                },
                TokenKind::DisjunctionConnective => unimplemented!(),
                TokenKind::ConditionalConnective => unimplemented!(),
                TokenKind::StatementSetOpening => {
                    // Statement sets begin with this token
                    match self.try_read_statement_set(&mut _iter) {
                        Ok(_) => continue,
                        Err(e)=> unimplemented!()
                    }
                },
                TokenKind::StatementSeparator => unimplemented!(),
                TokenKind::StatementSetClosing => unimplemented!(),
                TokenKind::ArgumentConclusionIndicator => {
                    // Argument conclusions begin with this token
                    match self.try_read_argument_conclusion(&mut _iter) {
                        Ok(_) => continue,
                        Err(e)=> unimplemented!()
                    }
                }
            }

            *iter = _iter.clone();

            self.token_iter.next();
        }

        if !found_match {
            // error
        }

        Ok(())
    }

    fn try_read_grouper(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_grouper");
        iter.try_read_token_of_kind(TokenKind::GrouperOpening)?;
        self.try_read_formula(iter)?;
        iter.try_read_token_of_kind(TokenKind::GrouperClosing)?;

        Ok(())
    }

    fn try_read_formula(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_formula");
        iter.try_read_token_of_kind(TokenKind::SimpleStatement).or_else(|_|
        self.try_read_complex_statement(iter))
    }

    fn try_read_logical_conjunction(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_logical_conjunction");
        iter.try_read_token_of_kind(TokenKind::GrouperOpening)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::ConjunctionConnective)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::GrouperClosing)?;

        Ok(())
    }

    fn try_read_logical_negation(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        iter.try_read_token_of_kind(TokenKind::NegationConnective)?;
        self.try_read_statement(iter)?;
        
        Ok(())
    }

    fn try_read_logical_disjunction(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        iter.try_read_token_of_kind(TokenKind::GrouperOpening)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::DisjunctionConnective)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::GrouperClosing)?;

        Ok(())
    }

    fn try_read_logical_conditional(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        iter.try_read_token_of_kind(TokenKind::GrouperOpening)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::ConditionalConnective)?;
        self.try_read_statement(iter)?;
        iter.try_read_token_of_kind(TokenKind::GrouperClosing)?;

        Ok(())
    }

    fn try_read_statement_set(&mut self, iter: &mut TokenIterator<'a>) -> Result<(), Error> {
        iter.try_read_token_of_kind(TokenKind::StatementSetOpening)?;
        self.read_until(iter, Some(TokenKind::StatementSetClosing))?;

        Ok(())
    }

    fn try_read_argument_conclusion(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        iter.try_read_token_of_kind(TokenKind::ArgumentConclusionIndicator)?;
        self.try_read_statement(iter)?;

        Ok(())
    }

    fn try_read_statement(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_statement");
        self.try_read_simple_statement(iter).or_else(|_|
        self.try_read_complex_statement(iter))
    }

    fn try_read_simple_statement(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_simple_statement");
        iter.try_read_token_of_kind(TokenKind::SimpleStatement)
    }

    fn try_read_complex_statement(&self, iter: &mut TokenIterator) -> Result<(), Error> {
        println!("try_read_complex_statement");
        self.try_read_logical_conjunction(iter).or_else(|_|
        self.try_read_logical_negation(iter)).or_else(|_|
        self.try_read_logical_disjunction(iter)).or_else(|_|
        self.try_read_logical_conditional(iter))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_valid_input() {
        let t = vec![
            Token::new(TokenKind::StatementSetOpening, TokenAssocData::StatementSetOpening(()), 0, 0),
            // Token::new(TokenKind::GrouperOpening, TokenAssocData::GrouperOpening(()), 0, 0),
            // Token::new(TokenKind::SimpleStatement, TokenAssocData::SimpleStatement('A', None), 0, 0),
            // Token::new(TokenKind::ConjunctionConnective, TokenAssocData::ConjunctionConnective(()), 0, 0),
            // Token::new(TokenKind::SimpleStatement, TokenAssocData::SimpleStatement('B', None), 0, 0),
            // Token::new(TokenKind::GrouperClosing, TokenAssocData::GrouperClosing(()), 0, 0),
            // Token::new(TokenKind::StatementSetClosing, TokenAssocData::StatementSetClosing(()), 0, 0)
        ];
        let mut p = Parser::new(&t);

        match p.validate() {
            Err(_) => assert!(false),
            _ => {}
        }
    }
}