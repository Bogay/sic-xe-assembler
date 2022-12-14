use crate::Rule;
use thiserror::Error;

#[derive(Debug, Error)]
pub(crate) enum ParseError<'a> {
    #[error("Invalid expression for {rule:?}. Got {expr}")]
    InvalidExpression { rule: Rule, expr: &'a str },
    #[error("Input is not a {expected:?}. Got {found:?}")]
    InvalidRule { expected: Rule, found: Rule },
}
