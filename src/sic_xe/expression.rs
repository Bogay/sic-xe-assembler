use super::{Command, Directive, Flag, Literal, Operand, Token};
use crate::Rule;
use itertools::Itertools;
use pest::iterators::Pair;
use std::fmt::{Display, Formatter};

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
