use std::str::FromStr;

use crate::Rule;
use pest::iterators::Pair;

#[derive(Debug, Clone, Copy)]
pub(crate) enum Format {
    One,
    Two,
    ThreeAndFour,
}

impl Format {
    pub fn len(&self) -> usize {
        match self {
            Self::One => 3,
            Self::Two => 2,
            Self::ThreeAndFour => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Mnemonic {
    name: &'static str,
    opcode: u8,
    format: Format,
}

impl Mnemonic {
    pub fn name(&self) -> &str {
        self.name
    }

    pub fn len(&self) -> usize {
        self.format.len()
    }

    pub fn opcode(&self) -> u8 {
        self.opcode
    }

    pub fn format(&self) -> Format {
        self.format
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Directive {
    Start,
    End,
    Byte,
    Word,
    ResB,
    ResW,
    Base,
}

impl Directive {
    pub fn name(&self) -> &str {
        match self {
            Directive::Start => "START",
            Directive::End => "END",
            Directive::Byte => "BYTE",
            Directive::Word => "WORD",
            Directive::ResB => "RESB",
            Directive::ResW => "RESW",
            Directive::Base => "BASE",
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Base | Self::Start | Self::End => 0,
            Self::Byte | Self::ResB => 1,
            Self::Word | Self::ResW => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Command {
    Mnemonic(Mnemonic),
    Directive(Directive),
}

impl Command {
    pub fn name(&self) -> &str {
        match self {
            Self::Mnemonic(m) => m.name(),
            Self::Directive(d) => d.name(),
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Command {
    type Error = &'a str;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        if !matches!(value.as_rule(), Rule::Command | Rule::Directive) {
            return Err("Input is not a command");
        }

        value.as_str().parse()
    }
}

impl FromStr for Command {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        macro_rules! decl_command {
            ($sym:ident) => {
                Self::Directive(Directive::$sym)
            };
            ($sym:ident, $opcode:expr, $format:ident) => {
                Self::Mnemonic(Mnemonic {
                    name: stringify!($sym),
                    opcode: $opcode,
                    format: Format::$format,
                })
            };
        }

        Ok(match s {
            "START" => decl_command!(Start),
            "END" => decl_command!(End),
            "BYTE" => decl_command!(Byte),
            "WORD" => decl_command!(Word),
            "RESB" => decl_command!(ResB),
            "RESW" => decl_command!(ResW),
            "BASE" => decl_command!(Base),
            "ADD" => decl_command!(ADD, 0x18, ThreeAndFour),
            "ADDF" => decl_command!(ADDF, 0x58, ThreeAndFour),
            "ADDR" => decl_command!(ADDR, 0x90, Two),
            "AND" => decl_command!(AND, 0x40, ThreeAndFour),
            "CLEAR" => decl_command!(CLEAR, 0xB4, Two),
            "COMP" => decl_command!(COMP, 0x28, ThreeAndFour),
            "COMPF" => decl_command!(COMPF, 0x88, ThreeAndFour),
            "COMPR" => decl_command!(COMPR, 0xA0, Two),
            "DIV" => decl_command!(DIV, 0x24, ThreeAndFour),
            "DIVF" => decl_command!(DIVF, 0x64, ThreeAndFour),
            "DIVR" => decl_command!(DIVR, 0x9C, Two),
            "FIX" => decl_command!(FIX, 0xC4, One),
            "FLOAT" => decl_command!(FLOAT, 0xC0, One),
            "HIO" => decl_command!(HIO, 0xF4, One),
            "J" => decl_command!(J, 0x3C, ThreeAndFour),
            "JEQ" => decl_command!(JEQ, 0x30, ThreeAndFour),
            "JGT" => decl_command!(JGT, 0x34, ThreeAndFour),
            "JLT" => decl_command!(JLT, 0x38, ThreeAndFour),
            "JSUB" => decl_command!(JSUB, 0x48, ThreeAndFour),
            "LDA" => decl_command!(LDA, 0x00, ThreeAndFour),
            "LDB" => decl_command!(LDB, 0x68, ThreeAndFour),
            "LDCH" => decl_command!(LDCH, 0x50, ThreeAndFour),
            "LDF" => decl_command!(LDF, 0x70, ThreeAndFour),
            "LDL" => decl_command!(LDL, 0x08, ThreeAndFour),
            "LDS" => decl_command!(LDS, 0x6C, ThreeAndFour),
            "LDT" => decl_command!(LDT, 0x74, ThreeAndFour),
            "LDX" => decl_command!(LDX, 0x04, ThreeAndFour),
            "LPS" => decl_command!(LPS, 0xD0, ThreeAndFour),
            "MUL" => decl_command!(MUL, 0x20, ThreeAndFour),
            "MULF" => decl_command!(MULF, 0x60, ThreeAndFour),
            "MULR" => decl_command!(MULR, 0x98, Two),
            "NORM" => decl_command!(NORM, 0xC8, One),
            "OR" => decl_command!(OR, 0x44, ThreeAndFour),
            "RD" => decl_command!(RD, 0xD8, ThreeAndFour),
            "RMO" => decl_command!(RMO, 0xAC, Two),
            "RSUB" => decl_command!(RSUB, 0x4C, One),
            "SHIFTL" => decl_command!(SHIFTL, 0xA4, Two),
            "SHIFTR" => decl_command!(SHIFTR, 0xA8, Two),
            "SIO" => decl_command!(SIO, 0xF0, One),
            "SSK" => decl_command!(SSK, 0xEC, ThreeAndFour),
            "STA" => decl_command!(STA, 0x0C, ThreeAndFour),
            "STB" => decl_command!(STB, 0x78, ThreeAndFour),
            "STCH" => decl_command!(STCH, 0x54, ThreeAndFour),
            "STF" => decl_command!(STF, 0x80, ThreeAndFour),
            "STI" => decl_command!(STI, 0xD4, ThreeAndFour),
            "STL" => decl_command!(STL, 0x14, ThreeAndFour),
            "STS" => decl_command!(STS, 0x7C, ThreeAndFour),
            "STSW" => decl_command!(STSW, 0xE8, ThreeAndFour),
            "STT" => decl_command!(STT, 0x84, ThreeAndFour),
            "STX" => decl_command!(STX, 0x10, ThreeAndFour),
            "SUB" => decl_command!(SUB, 0x1C, ThreeAndFour),
            "SUBF" => decl_command!(SUBF, 0x5C, ThreeAndFour),
            "SUBR" => decl_command!(SUBR, 0x94, Two),
            "SVC" => decl_command!(SVC, 0xB0, Two),
            "TD" => decl_command!(TD, 0xE0, ThreeAndFour),
            "TIO" => decl_command!(TIO, 0xF8, One),
            "TIX" => decl_command!(TIX, 0x2C, ThreeAndFour),
            "TIXR" => decl_command!(TIXR, 0xB8, Two),
            "WD" => decl_command!(WD, 0xDC, ThreeAndFour),
            _ => return Err("Unknown command"),
        })
    }
}
