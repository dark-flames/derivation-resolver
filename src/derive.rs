use crate::error::Error;
use crate::utils::error_pos;
use crate::visitor::{Visitable, Visitor};
use pest::error::Error as PestError;
use pest::iterators::{Pair, Pairs};
use pest::{Position, RuleType};
use std::any::type_name;
use std::result::Result as StdResult;

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

pub trait Judgement: Visitable {}

pub trait Parse<R: RuleType> {
    fn parse(entry_pair: Pair<R>) -> StdResult<Self, PestError<R>>
    where
        Self: Sized;
}

pub trait ParseAs<'i, R: RuleType, T: Sized> {
    fn parse(self) -> StdResult<T, PestError<R>>
    where
        Self: Sized;

    fn parse_with_pos(self) -> StdResult<(T, Position<'i>), PestError<R>>
    where
        Self: Sized;
}

pub trait ParseNextAs<'i, R: RuleType, T: Sized> {
    fn parse_next(&mut self, pos: Position) -> StdResult<T, PestError<R>>;

    fn parse_next_with_pos(&mut self, pos: Position) -> StdResult<(T, Position<'i>), PestError<R>>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DerivationTree<J: Judgement> {
    pub judgement: J,
    pub reason: RuleName,
    pub premises: Vec<DerivationTree<J>>,
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

impl<R: RuleType, T: Parse<R>> Parse<R> for Box<T> {
    fn parse(entry_pair: Pair<R>) -> StdResult<Self, PestError<R>>
    where
        Self: Sized,
    {
        entry_pair.parse().map(Box::new)
    }
}

impl<'i, R: RuleType, T: Parse<R>> ParseAs<'i, R, T> for Pair<'i, R> {
    fn parse(self) -> StdResult<T, PestError<R>>
    where
        Self: Sized,
    {
        T::parse(self)
    }

    fn parse_with_pos(self) -> StdResult<(T, Position<'i>), PestError<R>>
    where
        Self: Sized,
    {
        let pos = self.as_span().end_pos();
        T::parse(self).map(move |r| (r, pos))
    }
}

impl<'i, R: RuleType, T: Parse<R>> ParseNextAs<'i, R, T> for Pairs<'i, R> {
    fn parse_next(&mut self, pos: Position) -> StdResult<T, PestError<R>> {
        self.next()
            .ok_or_else(|| {
                error_pos(
                    pos,
                    format!("expect a/an {} here, got nothing", type_name::<T>()),
                )
            })
            .and_then(ParseAs::parse)
    }

    fn parse_next_with_pos(&mut self, pos: Position) -> StdResult<(T, Position<'i>), PestError<R>> {
        self.next()
            .ok_or_else(|| {
                error_pos(
                    pos,
                    format!("expect a/an {} here, got nothing", type_name::<T>()),
                )
            })
            .and_then(ParseAs::parse_with_pos)
    }
}

impl<J: Judgement> Visitable for DerivationTree<J> {
    fn apply_visitor<R, T: Visitor<Self, R>>(&self, visitor: &mut T) -> R
    where
        Self: Sized,
    {
        visitor.visit(self)
    }
}
