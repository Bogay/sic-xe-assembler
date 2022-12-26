use crate::Rule;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError<'a> {
    #[error("Invalid expression for {rule:?}. Got {expr}")]
    InvalidExpression { rule: Rule, expr: &'a str },
    #[error("Input is not a {expected:?}. Got {found:?}")]
    InvalidRule { expected: Rule, found: Rule },
    #[error("Duplicated symbol {0}")]
    DupSymbol(String),
    #[error("Uncategorized error: {0}")]
    Custom(&'a str),
}
