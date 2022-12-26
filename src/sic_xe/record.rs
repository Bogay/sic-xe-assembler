use super::{Command, Directive, Expression, Literal, Operand};
use std::fmt::{Display, Formatter};

#[derive(Debug, Default)]
pub(crate) struct Text<'a> {
    pub expressions: Vec<(Expression<'a>, String)>,
    pub start_addr: u64,
    pub len: u64,
}

impl<'a> Text<'a> {
    pub fn expressions(&self) -> &[(Expression<'a>, String)] {
        &self.expressions
    }
}

impl<'a> Display for Text<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self {
            start_addr, len, ..
        } = self;
        let mut str = String::new();
        for (_, objcode) in &self.expressions {
            str += objcode;
        }
        write!(f, "T{start_addr:<06X}{len:<02X}{str}")
    }
}

#[derive(Debug)]
pub(crate) struct End {
    pub start_addr: u64,
}

impl Display for End {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:06X}", self.start_addr)
    }
}

#[derive(Debug)]
pub(crate) struct Header<'a> {
    pub program_name: &'a str,
    pub start_addr: u64,
    pub len: u64,
}

impl<'a> Header<'a> {
    pub(crate) fn start_addr(&self) -> u64 {
        self.start_addr
    }
}

impl<'a> Display for Header<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self {
            program_name,
            start_addr,
            len,
        } = self;
        write!(f, "H{program_name:6}{start_addr:06X}{len:06X}")
    }
}

impl<'a> TryFrom<Expression<'a>> for Header<'a> {
    type Error = &'a str;

    fn try_from(expr: Expression<'a>) -> Result<Self, Self::Error> {
        if !matches!(expr.command(), Command::Directive(Directive::Start)) {
            return Err("Input should be a START directive");
        }
        let Some(Operand::Literal(Literal::Integer(start_addr))) = expr.operand() else {
            return Err("Invalid literal. Expect a integer");
        };
        let Some(program_name) = expr.label() else {
            return Err("Missing program name");
        };

        Ok(Self {
            program_name,
            start_addr: u64::from_str_radix(&start_addr.to_string(), 16).unwrap(),
            len: 0,
        })
    }
}
