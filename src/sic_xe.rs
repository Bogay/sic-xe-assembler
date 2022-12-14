use crate::{
    command::{Command, Directive, Format},
    Rule,
};
use bitflags::bitflags;
use itertools::Itertools;
use pest::iterators::Pair;
use std::{
    collections::{hash_map, HashMap},
    fmt::{Display, Formatter},
    io::{self, Write},
    str::FromStr,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub(crate) enum ParseError<'a> {
    #[error("Invalid expression for {rule:?}. Got {expr}")]
    InvalidExpression { rule: Rule, expr: &'a str },
    #[error("Input is not a {expected:?}. Got {found:?}")]
    InvalidRule { expected: Rule, found: Rule },
}

#[derive(Debug)]
pub(crate) struct SicXeProgram<'a> {
    expressions: Vec<Expression<'a>>,
    symbol_table: HashMap<&'a str, u64>,
    header: Header<'a>,
    end: End,
    texts: Vec<Text<'a>>,
}

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
    pub(crate) struct Flag: u8 {
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

#[derive(Debug, Clone)]
pub(crate) struct Expression<'a> {
    command: Command,
    operand: Option<Operand<'a>>,
    label: Option<&'a str>,
    stat: Flag,
}

impl<'a> Expression<'a> {
    pub fn len(&self) -> usize {
        match &self.command {
            Command::Mnemonic(mnemonic) => mnemonic.len() + self.stat.contains(Flag::E) as usize,
            Command::Directive(directive) => match directive {
                Directive::ResW | Directive::ResB => {
                    if let Some(Operand::Literal(Literal::Integer(size))) = self.operand {
                        directive.len() * (size as usize)
                    } else {
                        directive.len()
                    }
                }
                Directive::Byte => {
                    if let Some(Operand::Literal(Literal::String(str))) = &self.operand {
                        directive.len() * str.len()
                    } else {
                        directive.len()
                    }
                }
                _ => directive.len(),
            },
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Expression<'a>
where
    Self: 'a,
{
    type Error = &'a str;

    // FIXME: validate stat
    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        let tokens = value
            .into_inner()
            .map(Token::parse_with_stat)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| {
                eprintln!("{e}");
                "Invalid expression. (parse token)"
            })?;

        let mut stat = Flag::empty();
        for (_, s) in &tokens {
            stat |= *s;
        }

        let tokens = tokens.into_iter().map(|(tk, _)| tk).collect::<Vec<_>>();

        match tokens.len() {
            1 => {
                if let Token::Command(command) = &tokens[0] {
                    Ok(Self {
                        command: command.clone(),
                        operand: None,
                        label: None,
                        stat,
                    })
                } else {
                    Err("Invalid expression. Single token shuold be command")
                }
            }
            2 => {
                let (tk0, tk1) = tokens.into_iter().collect_tuple().unwrap();
                // label (symbol) + command
                if let (Token::Symbol(label), Token::Command(command)) = (&tk0, &tk1) {
                    Ok(Self {
                        command: command.clone(),
                        operand: None,
                        label: Some(label),
                        stat,
                    })
                    // command + operand (symbol || literal || register_pair)
                } else if let (Token::Command(command), Ok(operand)) =
                    (&tk0, Operand::try_from(tk1.clone()))
                {
                    Ok(Self {
                        command: command.clone(),
                        operand: Some(operand),
                        label: None,
                        stat,
                    })
                } else {
                    Err("Invalid expression")
                }
            }
            3 => {
                let (tk0, tk1, tk2) = tokens.into_iter().collect_tuple().unwrap();
                // FIXME: return err
                let operand = Operand::try_from(tk2.clone()).unwrap();
                if let (Token::Symbol(lable), Token::Command(command)) = (&tk0, &tk1) {
                    Ok(Self {
                        command: command.clone(),
                        operand: Some(operand),
                        label: Some(lable),
                        stat,
                    })
                } else {
                    Err("Invalid expression")
                }
            }
            _ => unreachable!("Invalid expression. Too many tokens in one line."),
        }
    }
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let label = self.label.unwrap_or_default();
        let operand = self
            .operand
            .as_ref()
            .map(|op| op.to_string())
            .unwrap_or_default();
        let command = self.command.name();
        write!(f, "{label: >12} {command: >12} {operand: >12}")
    }
}

#[derive(Debug)]
pub(crate) struct Header<'a> {
    program_name: &'a str,
    start_addr: u64,
    len: u64,
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
        if !matches!(expr.command, Command::Directive(Directive::Start)) {
            return Err("Input should be a START directive");
        }
        let Some(Operand::Literal(Literal::Integer(start_addr))) = expr.operand else {
            return Err("Invalid literal. Expect a integer");
        };
        let Some(program_name) = expr.label else {
            return Err("Missing program name");
        };

        Ok(Self {
            program_name,
            start_addr: u64::from_str_radix(&start_addr.to_string(), 16).unwrap(),
            len: 0,
        })
    }
}

#[derive(Debug)]
pub(crate) struct End {
    start_addr: u64,
}

impl Display for End {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:06X}", self.start_addr)
    }
}

#[derive(Debug, Default)]
pub(crate) struct Text<'a> {
    expressions: Vec<(Expression<'a>, String)>,
    start_addr: u64,
    len: u64,
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
        for (_, opcode) in &self.expressions {
            str += opcode;
        }
        write!(f, "T{start_addr:<06X}{len:<02X}{str}")
    }
}

fn to_hex(s: &[u8]) -> String {
    let mut ret = String::with_capacity(s.len() * 2);
    for b in s {
        ret.push_str(&format!("{b:02X}"));
    }
    ret
}

impl<'a> SicXeProgram<'a> {
    fn build_symbol_table(
        expressions: &[Expression<'a>],
        start_addr: u64,
    ) -> Result<HashMap<&'a str, u64>, &'a str> {
        let mut table = HashMap::new();
        let mut addr = start_addr;

        for expr in expressions {
            if let Some(label) = expr.label {
                match table.entry(label) {
                    hash_map::Entry::Occupied(_) => return Err("Duplicated symbol"),
                    hash_map::Entry::Vacant(ent) => {
                        ent.insert(addr);
                    }
                }
            }
            addr += expr.len() as u64;
        }

        Ok(table)
    }

    fn parse(
        expressions: &[Expression],
        symbol_table: &HashMap<&str, u64>,
        start_addr: u64,
    ) -> Result<Vec<String>, &'a str> {
        let mut pc = start_addr;
        let mut ret = vec![];
        let mut base = None;

        for expr in &expressions[1..expressions.len() - 1] {
            pc += expr.len() as u64;

            match &expr.command {
                Command::Directive(directive) => match directive {
                    Directive::ResB | Directive::ResW => ret.push("".to_string()),
                    Directive::Byte => {
                        let operand = expr.operand.as_ref().unwrap();
                        match operand {
                            Operand::Literal(lit) => match lit {
                                Literal::String(s) => ret.push(to_hex(s.as_bytes())),
                                Literal::Integer(i) => ret.push(format!("{i:02X}")),
                            },
                            _ => return Err("Invalid expression. BYTE should follows literal"),
                        }
                    }
                    Directive::Word => {
                        let operand = expr.operand.as_ref().unwrap();
                        let Operand::Literal(Literal::Integer(n)) = operand  else {
                            return Err("Invalid expression. WORD should follows integer")
                        };
                        ret.push(format!("{n:04X}"));
                    }
                    Directive::Base => {
                        let operand = expr.operand.as_ref().unwrap();
                        match operand {
                            Operand::Symbol(sym) => {
                                if let Some(addr) = symbol_table.get(sym) {
                                    base = Some(*addr);
                                    ret.push("".to_string());
                                } else {
                                    return Err("Symbol not found");
                                }
                            }
                            _ => return Err("Invalid expression. BASE should follows symbol"),
                        }
                    }
                    Directive::Start | Directive::End => return Err("Invalid program"),
                },
                Command::Mnemonic(mnemonic) => {
                    let mut code = 0u32;
                    let opcode = mnemonic.opcode() as u32;

                    match mnemonic.format() {
                        Format::One => {
                            code |= opcode << 16;
                            ret.push(format!("{: <02X}", code));
                        }
                        Format::Two => {
                            code |= opcode << 8;
                            let Operand::RegisterPair((r1, r2)) = expr.operand.as_ref().unwrap() else {
                                return Err("Invalid expression. Format two instruction should follows register pair");
                             };

                            code |= ((*r1 as u32) << 4) | (*r2 as u32);
                            ret.push(format!("{: <04X}", code));
                        }
                        Format::ThreeAndFour => {
                            if expr.stat.contains(Flag::E) {
                                code |= (opcode & 0xfc) << 24;
                            } else {
                                code |= (opcode & 0xfc) << 16;
                            }

                            let mut stat = expr.stat;
                            let addr;

                            match expr.operand.as_ref().unwrap() {
                                Operand::Literal(Literal::Integer(n)) => {
                                    addr = *n as u32;
                                }
                                Operand::Symbol(sym) => {
                                    let Some(address) = symbol_table.get(sym) else { return Err("Symbol not found") };
                                    if expr.stat.contains(Flag::E) {
                                        addr = (*address as u32) & ((1u32 << 20) - 1) as u32;
                                    } else {
                                        let (bias, flag) = Self::get_addr(*address, pc, base);

                                        if let Some(flag) = flag {
                                            stat |= flag;
                                            addr = (bias as u32) & ((1u32 << 12) - 1) as u32;
                                        } else {
                                            addr = (bias as u32) & ((1u32 << 12) - 1) as u32;
                                        }
                                    }
                                }
                                _ => return Err("Invalid expression. register pair?"),
                            }

                            if expr.stat.contains(Flag::E) {
                                code |= (stat.bits as u32) << 20;
                                code |= addr;
                                ret.push(format!("{: <08X}", code));
                            } else {
                                code |= (stat.bits as u32) << 12;
                                code |= addr;
                                ret.push(format!("{: <06X}", code));
                            }
                        }
                    }
                }
            }
        }

        Ok(ret)
    }

    fn get_addr(address: u64, pc: u64, base: Option<u64>) -> (i64, Option<Flag>) {
        let addr = address as i64;
        let pc = pc as i64;

        if (addr - pc) >= -2048 && (addr - pc) <= 2047 {
            return (addr - pc, Some(Flag::P));
        }

        let Some(base) = base else { return (addr, None)};
        let base = base as i64;
        if (addr >= base) && addr - base <= 4095 {
            (addr - base, Some(Flag::B))
        } else {
            (addr, None)
        }
    }

    pub fn texts(&self) -> &[Text] {
        self.texts.as_ref()
    }

    pub fn header(&self) -> &Header<'a> {
        &self.header
    }

    pub fn end(&self) -> &End {
        &self.end
    }

    pub fn symbol_table(&self) -> &HashMap<&'a str, u64> {
        &self.symbol_table
    }

    pub fn prettry_print(&self, mut f: impl Write) -> io::Result<()> {
        writeln!(
            f,
            "{:>04} {:>08} {:>12} {:>12} {:>12} opcode",
            "line", "address", "label", "operate", "operand"
        )?;

        let mut addr = self.header().start_addr();
        let mut line = 1;
        for text in self.texts() {
            for (expr, opcode) in text.expressions() {
                writeln!(f, "{:>04} {:>8X} {} {:>}", line, addr, expr, opcode)?;
                addr += expr.len() as u64;
                line += 1;
            }
        }

        Ok(())
    }

    pub fn opcode(&self) -> String {
        [format!("{}\n", self.header())]
            .into_iter()
            .chain(self.texts().iter().map(|text| format!("{}\n", text)))
            .chain([format!("{}", self.end())])
            .collect()
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for SicXeProgram<'a> {
    type Error = &'a str;

    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        if value.as_rule() != Rule::Program {
            return Err("Input is not a program");
        }

        let expressions = value
            .into_inner()
            .filter(|pair| match pair.as_rule() {
                Rule::Expression => true,
                Rule::EOI => false,
                _ => unreachable!("Parse expression"),
            })
            .map(|pair| match pair.as_rule() {
                Rule::Expression => Expression::try_from(pair),
                _ => unreachable!("Parse expression"),
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|err| {
                eprintln!("{err}");
                "Failed to parse expressions."
            })?;

        let mut header = Header::try_from(expressions[0].clone())?;
        let symbol_table = Self::build_symbol_table(&expressions, header.start_addr)?;
        let opcodes = Self::parse(&expressions, &symbol_table, header.start_addr)?;
        // FIXME: build end
        let sym = match expressions.last().unwrap().operand.as_ref().unwrap() {
            Operand::Symbol(sym) => sym,
            _ => panic!(),
        };
        let start_addr = *symbol_table.get(sym).unwrap();
        let end = End { start_addr };

        let mut sections = vec![];
        let mut cur_txt = Text {
            start_addr,
            ..Default::default()
        };
        let mut cur_addr = header.start_addr;
        let mut has_no_resv = false;

        for i in 1..expressions.len() - 1 {
            if cur_addr - cur_txt.start_addr + (expressions[i].len() as u64) > 0x1d && has_no_resv {
                cur_txt.len = cur_addr - cur_txt.start_addr;
                sections.push(cur_txt);
                cur_txt = Text {
                    start_addr: cur_addr,
                    ..Default::default()
                };
                has_no_resv = true;
            }

            match &expressions[i].command {
                Command::Directive(Directive::ResW) | Command::Directive(Directive::ResB) => {
                    if has_no_resv {
                        cur_txt.len = cur_addr - cur_txt.start_addr;
                        sections.push(cur_txt);
                        cur_txt = Text {
                            start_addr: cur_addr,
                            ..Default::default()
                        };
                        has_no_resv = false;
                    }
                }
                _ => {
                    if !has_no_resv {
                        cur_txt.start_addr = cur_addr;
                    }
                    has_no_resv = true;
                }
            }

            cur_txt
                .expressions
                .push((expressions[i].clone(), opcodes[i - 1].clone()));
            cur_addr += expressions[i].len() as u64;
        }

        if !cur_txt.expressions.is_empty() {
            cur_txt.len = cur_addr - cur_txt.start_addr;
            sections.push(cur_txt);
        }

        header.len = cur_addr;

        Ok(Self {
            expressions,
            symbol_table,
            header,
            end,
            texts: sections,
        })
    }
}
