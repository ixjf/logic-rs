// Because Rust's enum variants are not types :(
#[derive(Copy, Clone)]
pub enum TokenKind {
    GrouperOpening,
    GrouperClosing,
    SimpleStatement,
    ConjunctionConnective,
    NegationConnective,
    DisjunctionConnective,
    ConditionalConnective,
    StatementSetOpening,
    StatementSeparator,
    StatementSetClosing,
    ArgumentConclusionIndicator
}

#[derive(Copy, Clone)]
pub enum TokenAssocData {
    GrouperOpening(()),
    GrouperClosing(()),
    SimpleStatement(char, Option<u32>),
    ConjunctionConnective(()),
    NegationConnective(()),
    DisjunctionConnective(()),
    ConditionalConnective(()),
    StatementSetOpening(()),
    StatementSeparator(()),
    StatementSetClosing(()),
    ArgumentConclusionIndicator(())
}

pub struct Token {
    kind: TokenKind,
    assoc_data: TokenAssocData,
    source_line: u32,
    source_col: u32
}

impl Token {
    pub fn new(kind: TokenKind, assoc_data: TokenAssocData, line: u32, col: u32) -> Self {
        Token { kind, assoc_data, source_line: line, source_col: col }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn assoc_data(&self) -> TokenAssocData {
        self.assoc_data
    }

    pub fn source_line(&self) -> u32 {
        self.source_line
    }

    pub fn source_column(&self) -> u32 {
        self.source_col
    }
}