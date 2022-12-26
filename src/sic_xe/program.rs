use super::{token::Register, Command, Directive, Expression, Flag, Format, Literal, Operand};
use crate::Rule;
use pest::iterators::Pair;
use std::{
    collections::{hash_map, HashMap},
    fmt::{Display, Formatter},
    io::{self, Write},
};

#[derive(Debug)]
pub struct SicXeProgram<'a> {
    expressions: Vec<Expression<'a>>,
    symbol_table: HashMap<&'a str, u64>,
    header: Header<'a>,
    end: End,
    texts: Vec<Text<'a>>,
}

impl<'a> SicXeProgram<'a> {
    fn build_symbol_table(
        expressions: &[Expression<'a>],
        start_addr: u64,
    ) -> Result<HashMap<&'a str, u64>, &'a str> {
        let mut table = HashMap::new();
        let mut addr = start_addr;

        for expr in expressions {
            if let Some(label) = expr.label() {
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

            match expr.command() {
                Command::Directive(directive) => match directive {
                    Directive::ResB | Directive::ResW => ret.push("".to_string()),
                    Directive::Byte => {
                        let operand = expr.operand().unwrap();
                        match operand {
                            Operand::Literal(lit) => ret.push(lit.opcode()),
                            _ => return Err("Invalid expression. BYTE should follows literal"),
                        }
                    }
                    Directive::Word => {
                        let operand = expr.operand().unwrap();
                        let Operand::Literal(Literal::Integer(n)) = operand  else {
                            return Err("Invalid expression. WORD should follows integer")
                        };
                        ret.push(format!("{n:06X}"));
                    }
                    Directive::Base => {
                        let operand = expr.operand().unwrap();
                        match operand {
                            Operand::Symbol(sym) => {
                                let Some(addr) = symbol_table.get(sym) else {
                                    return Err("Symbol not found");
                                };
                                base = Some(*addr);
                                ret.push("".to_string());
                            }
                            _ => return Err("Invalid expression. BASE should follows symbol"),
                        }
                    }
                    Directive::Start | Directive::End => return Err("Invalid program"),
                    _ => todo!(),
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
                            let (r1, r2) = match expr.operand().unwrap() {
                                Operand::RegisterPair((r1, r2)) => (r1, r2),
                                Operand::Register(reg) => (reg, &Register::A),
                                _ => return Err("Invalid expression. Format two instruction should follows register"),
                            };
                            code |= ((*r1 as u32) << 4) | (*r2 as u32);
                            ret.push(format!("{: <04X}", code));
                        }
                        Format::ThreeAndFour => {
                            let is_format_4 = expr.is_set(Flag::E);
                            if is_format_4 {
                                code |= (opcode & 0xfc) << 24;
                            } else {
                                code |= (opcode & 0xfc) << 16;
                            }

                            let mut stat = expr.stat();
                            let addr = {
                                if stat.contains(Flag::I) {
                                    // immediate mode
                                    let disp = match expr.operand().unwrap() {
                                        Operand::Literal(lit) => match lit {
                                            Literal::Integer(n) => *n as u32,
                                            Literal::Byte(b) => *b as u32,
                                            _ => return Err("Register"),
                                        },
                                        Operand::Symbol(sym) => {
                                            let Some(address) = symbol_table.get(sym) else { return Err("Symbol not found") };
                                            if is_format_4 {
                                                (*address as u32) & ((1u32 << 20) - 1) as u32
                                            } else {
                                                let (bias, flag) =
                                                    Self::get_addr(*address, pc, base);
                                                if let Some(flag) = flag {
                                                    stat |= flag;
                                                }
                                                (bias as u32) & ((1u32 << 12) - 1) as u32
                                            }
                                        }
                                        _ => return Err("Register"),
                                    };
                                    disp
                                } else if stat.contains(Flag::N) {
                                    // indirect addressing
                                    match expr.operand().unwrap() {
                                        Operand::Symbol(sym) => {
                                            let Some(address) = symbol_table.get(sym) else { return Err("Symbol not found") };
                                            if is_format_4 {
                                                (*address as u32) & ((1u32 << 20) - 1) as u32
                                            } else {
                                                let (bias, flag) =
                                                    Self::get_addr(*address, pc, base);
                                                if let Some(flag) = flag {
                                                    stat |= flag;
                                                }
                                                (bias as u32) & ((1u32 << 12) - 1) as u32
                                            }
                                        }
                                        _ => return Err("Not a symbol"),
                                    }
                                } else if stat.contains(Flag::X) {
                                    // index
                                    stat |= Flag::N | Flag::I;
                                    match expr.operand().unwrap() {
                                        Operand::Symbol(sym) => {
                                            let Some(address) = symbol_table.get(sym) else { return Err("Symbol not found") };
                                            if is_format_4 {
                                                (*address as u32) & ((1u32 << 20) - 1) as u32
                                            } else {
                                                let (bias, flag) =
                                                    Self::get_addr(*address, pc, base);
                                                if let Some(flag) = flag {
                                                    stat |= flag;
                                                }
                                                (bias as u32) & ((1u32 << 12) - 1) as u32
                                            }
                                        }
                                        _ => return Err("Not a symbol"),
                                    }
                                } else {
                                    // simple
                                    stat |= Flag::N | Flag::I;
                                    match expr.operand().unwrap() {
                                        Operand::Literal(lit) => match lit {
                                            Literal::Integer(n) => *n as u32,
                                            Literal::Byte(b) => *b as u32,
                                            _ => todo!(),
                                        },
                                        Operand::Symbol(sym) => {
                                            let Some(address) = symbol_table.get(sym) else { return Err("Symbol not found") };
                                            if is_format_4 {
                                                (*address as u32) & ((1u32 << 20) - 1) as u32
                                            } else {
                                                let (bias, flag) =
                                                    Self::get_addr(*address, pc, base);
                                                if let Some(flag) = flag {
                                                    stat |= flag;
                                                }
                                                (bias as u32) & ((1u32 << 12) - 1) as u32
                                            }
                                        }
                                        _ => return Err("Invalid expression. register pair?"),
                                    }
                                }
                            };

                            if is_format_4 {
                                code |= (stat.bits() as u32) << 20;
                                code |= addr;
                                ret.push(format!("{: <08X}", code));
                            } else {
                                code |= (stat.bits() as u32) << 12;
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

    pub fn expressions(&self) -> &[Expression<'a>] {
        &self.expressions
    }

    pub(crate) fn texts(&self) -> &[Text] {
        self.texts.as_ref()
    }

    pub(crate) fn header(&self) -> &Header<'a> {
        &self.header
    }

    pub(crate) fn end(&self) -> &End {
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

    pub fn object_code(&self) -> String {
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
        let sym = match expressions.last().unwrap().operand().unwrap() {
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

            match expressions[i].command() {
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

#[derive(Debug)]
pub(crate) struct End {
    start_addr: u64,
}

impl Display for End {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{:06X}", self.start_addr)
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
