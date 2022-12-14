use super::{Command, ParseError};
use crate::Rule;
use bitflags::bitflags;
use itertools::Itertools;
use pest::iterators::Pair;
use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

#[derive(Debug, Clone)]
pub(crate) enum Token<'a> {
    Literal(Literal<'a>),
    Symbol(&'a str),
    Command(Command),
    RegisterPair((Register, Register)),
}

#[derive(Debug, Clone)]
pub(crate) enum Literal<'a> {
    String(&'a str),
    Integer(i32),
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{s}"),
            Literal::Integer(i) => write!(f, "{i}"),
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Literal<'a> {
    type Error = &'a str;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        if value.as_rule() != Rule::Literal {
            return Err("Input is not a literal");
        }

        let value = value.as_str();

        if value.starts_with('C') {
            let value = &value[2..value.len() - 1];
            Ok(Self::String(value))
        } else if value.starts_with('X') {
            let value = &value[2..value.len() - 1];
            let value =
                i32::from_str_radix(value, 16).map_err(|_| "Failed to parse integer literal")?;
            Ok(Self::Integer(value))
        } else {
            let value = value
                .parse()
                .map_err(|_| "Failed to parse integer literal")?;
            Ok(Self::Integer(value))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Register {
    A = 0,
    X = 1,
    L = 2,
    B = 3,
    S = 4,
    T = 5,
    F = 6,
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::A => write!(f, "A"),
            Register::X => write!(f, "X"),
            Register::L => write!(f, "L"),
            Register::B => write!(f, "B"),
            Register::S => write!(f, "S"),
            Register::T => write!(f, "T"),
            Register::F => write!(f, "F"),
        }
    }
}

impl FromStr for Register {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::A),
            "X" => Ok(Self::X),
            "L" => Ok(Self::L),
            "B" => Ok(Self::B),
            "S" => Ok(Self::S),
            "T" => Ok(Self::T),
            "F" => Ok(Self::F),
            _ => Err("Unknown register"),
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Register {
    type Error = &'a str;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        if value.as_rule() != Rule::Register {
            return Err("Input is not a register");
        }

        value.as_str().parse()
    }
}

bitflags! {
    pub struct Flag: u8 {
        const E = 0b00000001;
        const P = 0b00000010;
        const B = 0b00000100;
        const X = 0b00001000;
        const I = 0b00010000;
        const N = 0b00100000;
    }
}

impl Flag {
    fn from_prefix(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::E),
            "@" => Some(Self::N),
            "#" => Some(Self::I),
            _ => None,
        }
    }
    fn from_suffix(s: &str) -> Option<Self> {
        match s {
            ",X" => Some(Self::X),
            _ => None,
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Flag {
    type Error = &'a str;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match value.as_rule() {
            Rule::PrefixFlag => {
                Self::from_prefix(value.as_str()).ok_or("Failed to parse prefix flag")
            }
            Rule::SuffixFlag => {
                Self::from_suffix(value.as_str()).ok_or("Failed to parse suffix flag")
            }
            _ => Err("Input is not a flag"),
        }
    }
}

impl<'a> Token<'a> {
    // FIXME: validate token
    pub fn parse_with_stat(value: Pair<'a, Rule>) -> Result<(Self, Flag), ParseError<'a>> {
        if value.as_rule() != Rule::Token {
            return Err(ParseError::InvalidRule {
                expected: Rule::Token,
                found: value.as_rule(),
            });
        }

        let mut stat = Flag::empty();
        let mut val = None;
        let expr = <&str>::clone(&value.as_str());

        for pair in value.into_inner() {
            match pair.as_rule() {
                Rule::PrefixFlag | Rule::SuffixFlag => {
                    let flag: Flag = pair.try_into().unwrap();
                    stat |= flag;
                }
                Rule::Literal | Rule::RegisterPair | Rule::Symbol | Rule::Command => {
                    if let Ok(command) = Command::from_str(pair.as_str()) {
                        val = Some(Self::Command(command));
                    } else if let Ok(lit) = Literal::try_from(pair.clone()) {
                        val = Some(Self::Literal(lit));
                    } else if let Some((reg_a, reg_b)) = pair.as_str().split(',').collect_tuple() {
                        if let (Ok(reg_a), Ok(reg_b)) = (reg_a.parse(), reg_b.parse()) {
                            val = Some(Self::RegisterPair((reg_a, reg_b)));
                        } else {
                            return Err(ParseError::InvalidExpression {
                                rule: Rule::RegisterPair,
                                expr: pair.as_str(),
                            });
                        }
                    } else if let Ok(reg) = pair.as_str().parse() {
                        let reg_b = Register::A;
                        val = Some(Self::RegisterPair((reg, reg_b)));
                    } else if pair.as_str().chars().all(|c| c.is_alphabetic()) {
                        val = Some(Self::Symbol(pair.as_str()));
                    } else {
                        return Err(ParseError::InvalidExpression {
                            rule: Rule::Token,
                            expr: pair.as_str(),
                        });
                    }
                }
                _ => unreachable!(),
            }
        }

        if let Some(val) = val {
            Ok((val, stat))
        } else {
            Err(ParseError::InvalidExpression {
                rule: Rule::Token,
                expr,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Operand<'a> {
    Literal(Literal<'a>),
    Symbol(&'a str),
    RegisterPair((Register, Register)),
}

impl<'a> Display for Operand<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Literal(lit) => write!(f, "{lit}"),
            Operand::Symbol(sym) => write!(f, "{sym}"),
            Operand::RegisterPair((a, b)) => write!(f, "({a}, {b})"),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for Operand<'a> {
    type Error = &'a str;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Literal(lit) => Ok(Self::Literal(lit.clone())),
            Token::Symbol(sym) => Ok(Self::Symbol(<&str>::clone(&sym))),
            Token::RegisterPair(regs) => Ok(Self::RegisterPair(regs)),
            Token::Command(_) => Err("Command can not be a operand"),
        }
    }
}
