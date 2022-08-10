use crate::error::Error;
use crate::utils::error_pos;
use crate::visitor::{MutVisitable, Visitable, Visitor};
use pest::error::Error as PestError;
use pest::iterators::Pair;
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

    fn parse_boxed(self) -> StdResult<Box<T>, PestError<R>>
    where
        Self: Sized,
    {
        self.parse().map(Box::new)
    }

    fn parse_boxed_with_pos(self) -> StdResult<(Box<T>, Position<'i>), PestError<R>>
    where
        Self: Sized,
    {
        self.parse_with_pos().map(|(r, p)| (Box::new(r), p))
    }
}

pub trait ParseNextAs<'i, R: RuleType, T: Sized> {
    fn parse_next(&mut self, pos: Position) -> StdResult<T, PestError<R>>;

    fn parse_next_with_pos(&mut self, pos: Position) -> StdResult<(T, Position<'i>), PestError<R>>;

    fn parse_boxed_next(&mut self, pos: Position) -> StdResult<Box<T>, PestError<R>> {
        self.parse_next(pos).map(Box::new)
    }

    fn parse_boxed_next_with_pos(
        &mut self,
        pos: Position,
    ) -> StdResult<(Box<T>, Position<'i>), PestError<R>> {
        self.parse_next_with_pos(pos).map(|(r, p)| (Box::new(r), p))
    }
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

impl<'i, R: RuleType, T: Parse<R>, I: Iterator<Item = Pair<'i, R>>> ParseNextAs<'i, R, T> for I {
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
impl<J: Judgement> MutVisitable for DerivationTree<J> {}
