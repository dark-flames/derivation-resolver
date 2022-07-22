use crate::error::Error;
use crate::print::TokenBuffer;
use crate::systems::common::env::{Env, NamedEnv};
use crate::systems::common::syntax::Ident;
use crate::systems::eval_ml_3::syntax::BoxedNode;
use crate::{derive, ToToken};
use std::fmt::{Result as FmtResult, Write};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Function<E: Env> {
    pub env: E,
    pub bind: Ident,
    pub body: BoxedNode,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct RecursiveFunction<E: Env> {
    pub env: E,
    pub ident: Ident,
    pub bind: Ident,
    pub body: BoxedNode,
}

impl<E: Env> ToToken for Function<E> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.parenthesized(&self.env)?;
        buffer.write_char('[')?;
        write!(buffer, "fun {} ->", self.bind)?;
        self.body.to_token(buffer)?;
        buffer.write_char(']')?;

        Ok(())
    }
}

impl<E: Env> ToToken for RecursiveFunction<E> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.parenthesized(&self.env)?;
        buffer.write_char('[')?;
        write!(buffer, "rec {} = fun {} ->", self.ident, self.bind)?;
        self.body.to_token(buffer)?;
        buffer.write_char(']')?;

        Ok(())
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(Box<Function<NamedEnv>>),
    RecFun(Box<RecursiveFunction<NamedEnv>>),
}

impl Value {
    pub fn try_get_int(&self) -> derive::Result<i64> {
        if let Value::Integer(r) = self {
            Ok(*r)
        } else {
            Err(Error::NonIntegerValue)
        }
    }

    pub fn try_get_bool(&self) -> derive::Result<bool> {
        if let Value::Boolean(r) = self {
            Ok(*r)
        } else {
            Err(Error::NonBooleanValue)
        }
    }
}

impl ToToken for Value {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        match self {
            Value::Integer(i) => write!(buffer, "{}", i),
            Value::Boolean(b) => write!(buffer, "{}", b),
            Value::Fun(f) => f.to_token(buffer),
            Value::RecFun(rf) => rf.to_token(buffer),
        }
    }
}
