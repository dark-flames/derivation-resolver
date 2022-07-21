use crate::error::Error;
use crate::print::{ToToken, TokenBuffer};
use crate::utils::error_span;
use pest::error::Error as PestError;
use pest::iterators::{Pair, Pairs};
use pest::{RuleType, Span};
use std::any::type_name;
use std::fmt::{Result as fmtResult, Write};
use std::result::Result as StdResult;
use std::str::FromStr;

pub type Result<T> = StdResult<T, Error>;
pub type RuleName = &'static str;

pub trait System {
    type Judgement: Judgement;
    fn derive(src: &str) -> Result<DerivationTree<Self::Judgement>>;
}

pub trait Derivable: Judgement {
    fn derive(self) -> Result<DerivationTree<Self>>
    where
        Self: Sized;
}

pub trait Judgement: ToToken {}

pub trait Parse<R: RuleType> {
    fn parse(entry_pair: Pair<R>) -> StdResult<Self, PestError<R>>
    where
        Self: Sized;
}

pub trait ParseAs<R: RuleType, T: Sized> {
    fn parse_as(self) -> StdResult<T, PestError<R>>
    where
        Self: Sized;
}

pub trait ParseNextAs<R: RuleType, T: Sized> {
    fn parse_next_as(&mut self, span: Span) -> StdResult<T, PestError<R>>;
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

impl<R: RuleType, T: FromStr> Parse<R> for T {
    fn parse(entry_pair: Pair<R>) -> StdResult<Self, PestError<R>>
    where
        Self: Sized,
    {
        entry_pair.as_str().parse().map_err(|_| {
            error_span(
                entry_pair.as_span(),
                format!("expect a/an {} here", type_name::<T>()),
            )
        })
    }
}

impl<'i, R: RuleType, T: Parse<R>> ParseAs<R, T> for Pair<'i, R> {
    fn parse_as(self) -> StdResult<T, PestError<R>>
    where
        Self: Sized,
    {
        T::parse(self)
    }
}

impl<'i, R: RuleType, T: Parse<R>> ParseNextAs<R, T> for Pairs<'i, R> {
    fn parse_next_as(&mut self, span: Span) -> StdResult<T, PestError<R>> {
        self.next()
            .ok_or_else(|| error_span(span, format!("expect a/an {} here", type_name::<T>())))?
            .parse_as()
    }
}
