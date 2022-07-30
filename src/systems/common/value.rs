use crate::derive::Result;
use crate::error::Error;
use crate::systems::common::env::Env;
use crate::systems::common::syntax::Ident;
use crate::visitor::Visitable;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Function<E: Env> {
    pub env: E,
    pub bind: Ident,
    pub body: Box<E::Ast>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct RecursiveFunction<E: Env> {
    pub env: E,
    pub ident: Ident,
    pub bind: Ident,
    pub body: Box<E::Ast>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ConcatList<E: Env> {
    pub lhs: Box<Value<E>>,
    pub rhs: Box<Value<E>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Value<E: Env> {
    Integer(i64),
    Boolean(bool),
    Fun(Box<Function<E>>),
    RecFun(Box<RecursiveFunction<E>>),
    NilList,
    ConcatList(ConcatList<E>),
}

impl<E: Env> Value<E> {
    pub fn try_get_int(&self) -> Result<i64> {
        if let Value::Integer(r) = self {
            Ok(*r)
        } else {
            Err(Error::NonIntegerValue)
        }
    }

    pub fn try_get_bool(&self) -> Result<bool> {
        if let Value::Boolean(r) = self {
            Ok(*r)
        } else {
            Err(Error::NonBooleanValue)
        }
    }
}

impl<E: Env> Visitable for Value<E> {}

impl<E: Env> Visitable for Function<E> {}

impl<E: Env> Visitable for RecursiveFunction<E> {}

impl<E: Env> Visitable for ConcatList<E> {}
