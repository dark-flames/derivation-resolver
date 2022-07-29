use crate::systems::common::syntax::{AstRoot, Ident};
use crate::systems::common::value::Value;
use crate::visitor::{Visitable, Visitor};
use std::fmt::Debug;

pub trait Env: Clone + Debug + Eq + PartialEq + Visitable {
    type Item;
    type Ast: AstRoot;

    fn collect(&self) -> Vec<&Self::Item>;

    fn append(self, item: Self::Item) -> Self;

    fn append_named(self, name: Ident, item: Value<Self>) -> Self;

    fn lookup<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(&Self::Item, &Self) -> Option<R>,
        back: B,
    ) -> Option<R>;

    fn lookup_named<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(Option<&Ident>, &Value<Self>, &Self) -> Option<R>,
        back: B,
    ) -> Option<R>;
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NamedEnv<Ast: AstRoot> {
    Terminal,
    Segment(Box<Self>, (Ident, Value<Self>)),
}

impl<Ast: AstRoot> Visitable for NamedEnv<Ast> {
    fn apply_visitor<R, T: Visitor<Self, R>>(&self, visitor: &mut T) -> R
    where
        Self: Sized,
    {
        visitor.visit(self)
    }
}

impl<Ast: AstRoot> Env for NamedEnv<Ast> {
    type Item = (Ident, Value<Self>);
    type Ast = Ast;

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

    fn append_named(self, name: Ident, item: Value<Self>) -> Self {
        NamedEnv::Segment(Box::new(self), (name, item))
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

    fn lookup_named<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(Option<&Ident>, &Value<Self>, &Self) -> Option<R>,
        back: B,
    ) -> Option<R> {
        match self {
            NamedEnv::Terminal => None,
            NamedEnv::Segment(next, (ident, value)) => {
                check(Some(ident), value, self).or_else(|| {
                    next.lookup_named(check, back.clone())
                        .map(|r| back(r, self))
                })
            }
        }
    }
}
