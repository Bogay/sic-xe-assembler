use super::{
    record, token::Register, Command, Directive, Expression, Flag, Format, Literal, Operand,
    ParseError,
};
use crate::Rule;
use itertools::Itertools;
use pest::iterators::Pair;
use std::{
    collections::{hash_map, HashMap, VecDeque},
    io::{self, Write},
};

#[derive(Debug)]
pub struct SicXeProgram<'a> {
    expressions: Vec<Expression<'a>>,
    symbol_table: HashMap<String, u64>,
    header: record::Header<'a>,
    end: record::End,
    texts: Vec<record::Text<'a>>,
}

impl<'a> SicXeProgram<'a> {
    fn build_symbol_table(
        expressions: Vec<Expression<'a>>,
        start_addr: u64,
    ) -> Result<(HashMap<String, u64>, Vec<Expression<'a>>), ParseError<'a>> {
        let mut table = HashMap::new();
        let mut addr = start_addr;
        let mut lit_pool: Vec<Expression> = vec![];

        let mut expressions = expressions
            .into_iter()
            .map(|expr| {
                let mut tmp_exprs = VecDeque::new();

                if matches!(expr.command(), Command::Directive(Directive::LtOrg)) {
                    for lit in lit_pool.drain(..) {
                        // Assign addr for each literal declaring expressions
                        let id = match lit.command() {
                            Command::DeclareLiteral { id, .. } => id.clone(),
                            _ => unreachable!("Build symbol for literal declaring expressions"),
                        };
                        match table.entry(id) {
                            // The expression has been created
                            hash_map::Entry::Occupied(_) => {
                                continue;
                            }
                            hash_map::Entry::Vacant(ent) => {
                                ent.insert(addr);
                            }
                        }
                        addr += lit.len() as u64;
                        tmp_exprs.push_back(lit);
                    }
                }

                if let Some(e) = expr.clone().extra_expression() {
                    lit_pool.push(e);
                }

                if let Some(label) = expr.label() {
                    match table.entry(label.to_string()) {
                        hash_map::Entry::Occupied(ent) => {
                            return Err(ParseError::DupSymbol(ent.key().to_string()))
                        }
                        hash_map::Entry::Vacant(ent) => {
                            ent.insert(addr);
                        }
                    }
                }

                addr += expr.len() as u64;
                tmp_exprs.push_front(expr);
                Ok(tmp_exprs)
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect_vec();

        let end = expressions.pop().unwrap();
        for lit in lit_pool.drain(..) {
            // Assign addr for each literal declaring expressions
            let id = match lit.command() {
                Command::DeclareLiteral { id, .. } => id.clone(),
                _ => unreachable!("Build symbol for literal declaring expressions"),
            };
            match table.entry(id) {
                // The expression has been created
                hash_map::Entry::Occupied(_) => {
                    continue;
                }
                hash_map::Entry::Vacant(ent) => {
                    ent.insert(addr);
                }
            }
            addr += lit.len() as u64;
            expressions.push(lit);
        }
        expressions.push(end);

        Ok((table, expressions))
    }

    fn build_objcodes(
        expressions: &[Expression],
        symbol_table: &HashMap<String, u64>,
        start_addr: u64,
    ) -> Result<Vec<String>, &'a str> {
        let mut pc = start_addr;
        let mut ret = vec![];
        let mut base = None;

        for (line, expr) in expressions[1..expressions.len() - 1].iter().enumerate() {
            pc += expr.len() as u64;

            match expr.command() {
                Command::Directive(directive) => match directive {
                    Directive::ResB | Directive::ResW | Directive::LtOrg => {
                        ret.push("".to_string())
                    }
                    Directive::Byte => {
                        let operand = expr.operand().unwrap();
                        match operand {
                            Operand::Literal(lit) => ret.push(lit.objcode()),
                            _ => return Err("Invalid expression. BYTE should follows literal"),
                        }
                    }
                    Directive::Word => {
                        let operand = expr.operand().unwrap();
                        let Operand::Literal(Literal::Integer(n)) = operand else {
                            return Err("Invalid expression. WORD should follows integer")
                        };
                        ret.push(format!("{n:06X}"));
                    }
                    Directive::Base => {
                        let operand = expr.operand().unwrap();
                        match operand {
                            Operand::Symbol(sym) => {
                                let sym = *sym;
                                let Some(addr) = symbol_table.get(sym) else {
                                    return Err("Symbol not found");
                                };
                                base = Some(*addr);
                                ret.push("".to_string());
                            }
                            _ => return Err("Invalid expression. BASE should follows symbol"),
                        }
                    }
                    Directive::Start | Directive::End => {
                        eprintln!("{}", line + 1);
                        return Err(
                        "Invalid program. START or END should not appear in the middle of program",
                    );
                    }
                },
                Command::Mnemonic(mnemonic) => {
                    let mut code = 0u32;
                    let objcode = mnemonic.opcode() as u32;

                    match mnemonic.format() {
                        Format::One => {
                            code |= objcode << 16;
                            ret.push(format!("{: <02X}", code));
                        }
                        Format::Two => {
                            code |= objcode << 8;
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
                                code |= (objcode & 0xfc) << 24;
                            } else {
                                code |= (objcode & 0xfc) << 16;
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
                                            let Some(address) = symbol_table.get(*sym) else { return Err("Symbol not found") };
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
                                            let Some(address) = symbol_table.get(*sym) else { return Err("Symbol not found") };
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
                                            let Some(address) = symbol_table.get(*sym) else { return Err("Symbol not found") };
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
                                            // TODO: Check whether string can be used here
                                            _ => panic!(),
                                        },
                                        Operand::Symbol(sym) => {
                                            let Some(address) = symbol_table.get(*sym) else { return Err("Symbol not found") };
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
                                        Operand::DeclareLiteral { id, .. } => {
                                            let Some(address) = symbol_table.get(id) else { return Err("Symbol not found") };
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
                Command::DeclareLiteral { lit, .. } => ret.push(lit.objcode()),
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

    pub(crate) fn texts(&self) -> &[record::Text] {
        self.texts.as_ref()
    }

    pub(crate) fn header(&self) -> &record::Header<'a> {
        &self.header
    }

    pub(crate) fn end(&self) -> &record::End {
        &self.end
    }

    pub fn symbol_table(&self) -> &HashMap<String, u64> {
        &self.symbol_table
    }

    pub fn pretty_print(&self, mut f: impl Write) -> io::Result<()> {
        writeln!(
            f,
            "{:>04}  {:>08}  {:<8}  {:<12}  {:<12}  object code",
            "line", "address", "label", "operate", "operand"
        )?;

        let mut addr = self.header().start_addr();
        let mut line = 1;
        // START
        writeln!(f, "{:>04}  {:>8X}  {}", line, addr, self.expressions()[0])?;
        line += 1;
        for text in self.texts() {
            for (expr, objcode) in text.expressions() {
                writeln!(f, "{:>04}  {:>8X}  {}  {:>}", line, addr, expr, objcode)?;
                addr += expr.len() as u64;
                line += 1;
            }
        }
        // END
        writeln!(
            f,
            "{:>04}  {: >8}  {}",
            line,
            "",
            self.expressions().last().unwrap()
        )?;

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
            .filter(|pair| matches!(pair.as_rule(), Rule::Expression))
            .map(Expression::try_from)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|err| {
                eprintln!("{err}");
                "Failed to parse expressions."
            })?;

        let mut header = record::Header::try_from(expressions[0].clone())?;
        let (symbol_table, expressions) = Self::build_symbol_table(expressions, header.start_addr)
            .map_err(|e| {
                eprintln!("{e}");
                "Build symbol table"
            })?;
        let objcodes = Self::build_objcodes(&expressions, &symbol_table, header.start_addr)?;
        // FIXME: build end
        let sym = match expressions.last().unwrap().operand().unwrap() {
            Operand::Symbol(sym) => sym,
            _ => panic!(),
        };
        let start_addr = *symbol_table.get(*sym).unwrap();
        let end = record::End { start_addr };

        let mut texts = vec![];
        let mut cur_txt = record::Text {
            start_addr,
            ..Default::default()
        };
        let mut cur_addr = header.start_addr;
        let mut has_no_resv = false;

        for (expr, objcode) in expressions[1..expressions.len() - 1].iter().zip(objcodes) {
            if cur_addr - cur_txt.start_addr + (expr.len() as u64) > 0x1d && has_no_resv {
                cur_txt.len = cur_addr - cur_txt.start_addr;
                texts.push(cur_txt);
                cur_txt = record::Text {
                    start_addr: cur_addr,
                    ..Default::default()
                };
                has_no_resv = true;
            }

            match expr.command() {
                Command::Directive(Directive::ResW) | Command::Directive(Directive::ResB) => {
                    if has_no_resv {
                        cur_txt.len = cur_addr - cur_txt.start_addr;
                        texts.push(cur_txt);
                        cur_txt = record::Text {
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

            cur_txt.expressions.push((expr.clone(), objcode));
            cur_addr += expr.len() as u64;
        }

        if !cur_txt.expressions.is_empty() {
            cur_txt.len = cur_addr - cur_txt.start_addr;
            texts.push(cur_txt);
        }

        header.len = cur_addr - header.start_addr();

        Ok(Self {
            expressions,
            symbol_table,
            header,
            end,
            texts,
        })
    }
}
