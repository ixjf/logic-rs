use super::pest_parser::Rule;
use pest::error::Error as pest_error;
use pest::error::ErrorVariant as pest_error_variant;
use pest::Span;
use std::fmt;

pub struct Error {
    pub location: (usize, usize),
    decorated_message: String,
}

impl Error {
    pub(in crate::parser) fn new_from_custom_error(span: Span, decorated_message: &str) -> Self {
        let e: pest_error<Rule> = pest_error::new_from_span(
            pest_error_variant::CustomError {
                message: decorated_message.to_owned(),
            },
            span.clone(),
        );

        Error {
            location: span.start_pos().line_col(),
            decorated_message: format!("{}", e),
        }
    }

    pub(in crate::parser) fn new_from_parsing_error(e: pest_error<Rule>) -> Error {
        use pest::error::LineColLocation;

        let location = match e.line_col {
            LineColLocation::Pos((line, col)) => (line, col),
            _ => unreachable!(), // is this actually unreachable? it's not documented
        };

        Error {
            location,
            decorated_message: format!("{}", e),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.decorated_message)
    }
}
