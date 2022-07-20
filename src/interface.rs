use crate::error::Error;
use crate::print::{ToToken, TokenBuffer};
use std::fmt::{Display, Formatter, Result as fmtResult, Write};

pub type Result<T> = std::result::Result<T, Error>;
pub type RuleName = &'static str;

pub trait Syntax: ToToken {}

pub trait Judgement: ToToken {
    type S: Syntax;
}

pub struct DerivationTree<J: Judgement> {
    judgement: J,
    reason: RuleName,
    premises: Vec<DerivationTree<J>>,
}

impl<J: Judgement> DerivationTree<J> {
    pub fn new(judgement: J, reason: RuleName, premises: Vec<DerivationTree<J>>) -> Self {
        DerivationTree {
            judgement,
            reason,
            premises,
        }
    }
}

impl<J: Judgement> ToToken for DerivationTree<J> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> fmtResult {
        self.judgement.to_token(buffer)?;
        write!(buffer, "by {}", self.reason)?;
        if self.premises.is_empty() {
            write!(buffer, "{{}}")
        } else {
            let mut block = buffer.sub_buffer();
            let last_index = self.premises.len() - 1;
            for (index, premise) in self.premises.iter().enumerate() {
                premise.to_token(&mut block)?;
                if index != last_index {
                    block.commit_line(true)?;
                }
            }
            buffer.commit_block(block.freeze()?)
        }
    }
}
