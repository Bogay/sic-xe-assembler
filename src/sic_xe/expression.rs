use super::{Command, Directive, Flag, Format, Literal, Operand};
use crate::Rule;
use itertools::Itertools;
use pest::iterators::Pair;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Expression<'a> {
    label: Option<&'a str>,
    command: Command,
    operand: Option<Operand<'a>>,
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

    pub fn label(&self) -> Option<&'a str> {
        self.label
    }

    pub fn command(&self) -> &Command {
        &self.command
    }

    pub fn operand(&self) -> Option<&Operand> {
        self.operand.as_ref()
    }

    pub fn is_set(&self, flag: Flag) -> bool {
        self.stat().contains(flag)
    }

    pub fn stat(&self) -> Flag {
        self.stat
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Expression<'a> {
    type Error = &'a str;

    // FIXME: validate stat
    fn try_from(value: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        if value.as_rule() != Rule::Expression {
            return Err("Input is not a expression");
        }

        let mut command = None;
        let mut label = None;
        let mut operand = None;
        let mut stat = Flag::empty();

        for pair in value.into_inner() {
            match pair.as_rule() {
                Rule::Operand => {
                    for ppair in pair.into_inner() {
                        match ppair.as_rule() {
                            Rule::ImmediateAddressingMode
                            | Rule::IndirectAddressingMode
                            | Rule::IndexingAddressingMode => {
                                // Safety: guaranteed by parser
                                stat |= ppair.as_str().parse().unwrap();
                            }
                            Rule::Literal => {
                                if let Ok(lit) = Literal::try_from(ppair) {
                                    operand = Some(Operand::Literal(lit));
                                } else {
                                    return Err("Failed to parse literal");
                                }
                            }
                            Rule::RegisterPair => {
                                let (reg_a, reg_b) =
                                    ppair.as_str().split(',').collect_tuple().unwrap();
                                if let (Ok(reg_a), Ok(reg_b)) = (reg_a.parse(), reg_b.parse()) {
                                    operand = Some(Operand::RegisterPair((reg_a, reg_b)));
                                } else {
                                    return Err("Failed to parse register pair");
                                }
                            }
                            Rule::Symbol => {
                                if let Some(Command::Mnemonic(mnemonic)) = &command {
                                    if mnemonic.format() == Format::Two {
                                        operand = Some(Operand::Register(
                                            ppair.as_str().parse().unwrap(),
                                        ));
                                        continue;
                                    }
                                }
                                operand = Some(Operand::Symbol(ppair.as_str()));
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Rule::Command => {
                    for ppair in pair.into_inner() {
                        match ppair.as_rule() {
                            Rule::ExtendedAddressingMode => {
                                // Safety: guaranteed by parser
                                stat |= ppair.as_str().parse().unwrap();
                            }
                            Rule::Symbol => {
                                if let Ok(cmd) = ppair.as_str().parse() {
                                    command = Some(cmd);
                                    // builder = builder.command(command);
                                } else {
                                    return Err("Unknown command");
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Rule::Symbol => {
                    let expr = pair.as_str();
                    if expr.parse::<Command>().is_ok() {
                        return Err("Label can not be the same as opcode or directive");
                    }
                    label = Some(expr);
                }
                _ => unreachable!(),
            }
        }

        Ok(Self {
            command: command.unwrap(),
            label,
            operand,
            stat,
        })
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

        let before_command = self
            .stat()
            .contains(Flag::E)
            .then_some("+")
            .unwrap_or_default();
        let mut before_operand = self.stat().contains(Flag::N).then_some("@");
        if before_operand.is_none() {
            before_operand = self.stat().contains(Flag::I).then_some("#");
        }
        let before_operand = before_operand.unwrap_or_default();
        let after_operand = self
            .stat()
            .contains(Flag::X)
            .then_some(",X")
            .unwrap_or_default();
        let command = format!("{before_command}{command}");
        let operand = format!("{before_operand}{operand}{after_operand}");

        write!(f, "{label: >12} {command: >12} {operand: >12}")
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::SicXeParser;
    use pest::Parser;

    #[test]
    fn label_is_not_allowed_eq_opcde_or_directive() {
        let expression = SicXeParser::parse(Rule::Expression, "ADD ADD #1")
            .unwrap()
            .next()
            .unwrap();
        Expression::try_from(expression).unwrap_err();
    }
}
