use crate::derive::{Judgement as JudgementTrait, Result};
use crate::error::Error;
use crate::print::ToToken;
use crate::print::TokenBuffer;
use crate::systems::common::env::Env;
use crate::systems::common::syntax::Op;
use crate::systems::common::value::Value;
use crate::systems::eval_ml_3::syntax::EvalML3Node;
use std::fmt::{Result as FmtResult, Write};

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Judgement<E: Env> {
    EvalTo(EvalToJudgement<E>),
    PlusIs(i64, i64, i64),
    MinusIs(i64, i64, i64),
    TimesIs(i64, i64, i64),
    LtIs(i64, i64, bool),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct EvalToJudgement<E: Env> {
    pub env: E,
    pub term: EvalML3Node,
    pub value: Value,
}

impl<E: Env> JudgementTrait for Judgement<E> {}

impl<E: Env> EvalToJudgement<E> {
    pub fn new(env: E, term: EvalML3Node, value: Value) -> Self {
        EvalToJudgement { env, term, value }
    }
}

impl<E: Env> Judgement<E> {
    pub fn from_op(op: Op, a: &Value, b: &Value, c: &Value) -> Result<Self> {
        Ok(match op {
            Op::Plus => Judgement::PlusIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Minus => Judgement::MinusIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Times => Judgement::TimesIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Lt => Judgement::LtIs(a.try_get_int()?, b.try_get_int()?, c.try_get_bool()?),
        })
    }

    pub fn eval_result(&self) -> Result<&Value> {
        match self {
            Judgement::EvalTo(e) => Some(&e.value),
            Judgement::PlusIs(_, _, _) => None,
            Judgement::MinusIs(_, _, _) => None,
            Judgement::TimesIs(_, _, _) => None,
            Judgement::LtIs(_, _, _) => None,
        }
        .ok_or(Error::NotEvalToJudgement)
    }
}

impl<E: Env> ToToken for EvalToJudgement<E> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        self.env.to_token(buffer)?;
        buffer.write_str("|-")?;
        self.term.to_token(buffer)?;
        buffer.write_str("evalto")?;
        self.value.to_token(buffer)
    }
}

impl<E: Env> ToToken for Judgement<E> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        match self {
            Judgement::EvalTo(eval_to) => eval_to.to_token(buffer),
            Judgement::PlusIs(a, b, c) => write!(buffer, "{} plus {} is {}", a, b, c),
            Judgement::MinusIs(a, b, c) => write!(buffer, "{} minus {} is {}", a, b, c),
            Judgement::TimesIs(a, b, c) => write!(buffer, "{} times {} is {}", a, b, c),
            Judgement::LtIs(a, b, c) => write!(buffer, "{} less than {} is {}", a, b, c),
        }
    }
}
