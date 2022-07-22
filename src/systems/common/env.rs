use crate::print::TokenBuffer;
use crate::systems::common::syntax::Ident;
use crate::systems::common::value::Value;
use crate::ToToken;
use std::fmt::{Result as FmtResult, Write};

pub trait Env: ToToken + Clone {
    type Item;

    fn collect(&self) -> Vec<&Self::Item>;

    fn append(self, item: Self::Item) -> Self;

    fn lookup<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(&Self::Item, &Self) -> Option<R>,
        back: B,
    ) -> Option<R>;
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NamedEnv {
    Terminal,
    Segment(Box<NamedEnv>, (Ident, Value)),
}

impl Env for NamedEnv {
    type Item = (Ident, Value);

    fn collect(&self) -> Vec<&Self::Item> {
        match self {
            NamedEnv::Terminal => vec![],
            NamedEnv::Segment(next, item) => {
                let mut result = next.collect();

                result.push(item);

                result
            }
        }
    }

    fn append(self, item: Self::Item) -> Self {
        NamedEnv::Segment(Box::new(self), item)
    }

    fn lookup<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(&Self::Item, &Self) -> Option<R>,
        back: B,
    ) -> Option<R> {
        match self {
            NamedEnv::Terminal => None,
            NamedEnv::Segment(next, item) => check(item, self)
                .or_else(|| next.lookup(check, back.clone()).map(|r| back(r, self))),
        }
    }
}

impl ToToken for NamedEnv {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.write_by_iter(
            self.collect(),
            |(id, value), b| {
                b.write_str(id)?;
                b.write_char('=')?;
                value.to_token(b)
            },
            |b| b.write_char(','),
        )
    }
}
