use crate::Rule;
use bitflags::bitflags;
use pest::iterators::Pair;
use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

fn to_hex(s: &[u8]) -> String {
    let mut ret = String::with_capacity(s.len() * 2);
    for b in s {
        ret.push_str(&format!("{b:02X}"));
    }
    ret
}

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    String(&'a str),
    Integer(i32),
    Byte(u8),
}

impl<'a> Literal<'a> {
    pub fn opcode(&self) -> String {
        match self {
            Literal::String(s) => to_hex(s.as_bytes()),
            Literal::Integer(i) => format!("{i:02X}"),
            Literal::Byte(b) => format!("{b:02X}"),
        }
    }
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "C'{s}'"),
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Byte(b) => write!(f, "X'{b:02X}'"),
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
                u8::from_str_radix(value, 16).map_err(|_| "Failed to parse byte literal")?;
            Ok(Self::Byte(value))
        } else {
            let value = value
                .parse()
                .map_err(|_| "Failed to parse integer literal")?;
            Ok(Self::Integer(value))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
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

impl FromStr for Flag {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::E),
            "@" => Ok(Self::N),
            "#" => Ok(Self::I),
            ",X" => Ok(Self::X),
            _ => Err("Unknown flag"),
        }
    }
}

impl Flag {
    pub fn is_valid(self) -> bool {
        if !(self & (Self::B | Self::P)).is_empty() {
            return false;
        }

        true
    }
}

#[derive(Debug, Clone)]
pub enum Operand<'a> {
    Literal(Literal<'a>),
    Symbol(&'a str),
    Register(Register),
    RegisterPair((Register, Register)),
}

impl<'a> Display for Operand<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Literal(lit) => write!(f, "{lit}"),
            Operand::Symbol(sym) => write!(f, "{sym}"),
            Operand::RegisterPair((a, b)) => write!(f, "{a},{b}"),
            Operand::Register(reg) => write!(f, "{reg}"),
        }
    }
}
