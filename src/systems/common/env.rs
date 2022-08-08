use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::systems::common::syntax::{AstRoot, Ident};
use crate::systems::common::ty::{FreeTypeVarVisitor, MonoType, PolyType};
use crate::systems::common::value::Value;
use crate::visitor::{MutVisitable, Visitable, Visitor};

pub trait Env: Clone + Debug + Eq + PartialEq + Visitable {
    type Item;
    type Ast: AstRoot;

    fn collect(&self) -> Vec<(&Ident, &Self::Item)>;

    fn append_named(self, name: Ident, item: Self::Item) -> Self;

    fn look_up(&self, name: &Ident) -> Option<&Self::Item>;
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
    type Item = Value<Self>;
    type Ast = Ast;

    fn collect(&self) -> Vec<(&Ident, &Self::Item)> {
        match self {
            NamedEnv::Terminal => vec![],
            NamedEnv::Segment(next, (ident, value)) => {
                let mut result = next.collect();

                result.push((ident, value));

                result
            }
        }
    }

    fn append_named(self, name: Ident, item: Self::Item) -> Self {
        NamedEnv::Segment(Box::new(self), (name, item))
    }

    fn look_up(&self, name: &Ident) -> Option<&Self::Item> {
        match self {
            NamedEnv::Terminal => None,
            NamedEnv::Segment(next, (ident, value)) => {
                if ident == name {
                    Some(value)
                } else {
                    next.look_up(name)
                }
            }
        }
    }
}

impl<Ast: AstRoot> NamedEnv<Ast> {
    pub fn lookup_and_back<R, B: Clone + Fn(R, &Self) -> R>(
        &self,
        check: impl Fn(Option<&Ident>, &Value<Self>, &Self) -> Option<R>,
        back: B,
    ) -> Option<R> {
        match self {
            NamedEnv::Terminal => None,
            NamedEnv::Segment(next, (ident, value)) => {
                check(Some(ident), value, self).or_else(|| {
                    next.lookup_and_back(check, back.clone())
                        .map(|r| back(r, self))
                })
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct TypedEnv<Ast: AstRoot> {
    pub env: HashMap<Ident, PolyType>,
    _marker: PhantomData<Ast>,
}

impl<Ast: AstRoot> Visitable for TypedEnv<Ast> {}
impl<Ast: AstRoot> MutVisitable for TypedEnv<Ast> {}

impl<Ast: AstRoot> Env for TypedEnv<Ast> {
    type Item = PolyType;
    type Ast = Ast;

    fn collect(&self) -> Vec<(&Ident, &Self::Item)> {
        self.env.iter().collect()
    }

    fn append_named(mut self, name: Ident, item: Self::Item) -> Self {
        self.env.insert(name, item);
        self
    }

    fn look_up(&self, name: &Ident) -> Option<&Self::Item> {
        self.env.get(name)
    }
}
impl<Ast: AstRoot> TypedEnv<Ast> {
    pub fn new() -> Self {
        TypedEnv {
            env: Default::default(),
            _marker: Default::default(),
        }
    }

    pub fn generalize(&self, ty: MonoType) -> PolyType {
        let mut visitor = FreeTypeVarVisitor::default();
        let free_vars = self.apply_visitor(&mut visitor);
        PolyType {
            binds: ty
                .apply_visitor(&mut visitor)
                .difference(&free_vars)
                .cloned()
                .collect(),
            ty,
        }
    }
}
