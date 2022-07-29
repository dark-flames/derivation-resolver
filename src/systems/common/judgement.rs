use crate::derive::{Judgement as JudgementTrait, Result};
use crate::error::Error;
use crate::systems::common::env::Env;
use crate::systems::common::syntax::Op;
use crate::systems::common::value::Value;
use crate::visitor::Visitable;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct PlusIsJudgement(pub i64, pub i64, pub i64);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MinusIsJudgement(pub i64, pub i64, pub i64);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TimesIsJudgement(pub i64, pub i64, pub i64);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LtIsJudgement(pub i64, pub i64, pub bool);

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Judgement<E: Env> {
    EvalTo(EvalToJudgement<E>),
    PlusIs(PlusIsJudgement),
    MinusIs(MinusIsJudgement),
    TimesIs(TimesIsJudgement),
    LtIs(LtIsJudgement),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct EvalToJudgement<E: Env> {
    pub env: E,
    pub term: E::Ast,
    pub value: Value<E>,
}

impl<E: Env> JudgementTrait for Judgement<E> {}

impl<E: Env> EvalToJudgement<E> {
    pub fn new(env: E, term: E::Ast, value: Value<E>) -> Self {
        EvalToJudgement { env, term, value }
    }
}

impl<E: Env> Judgement<E> {
    pub fn from_op(op: Op, a: &Value<E>, b: &Value<E>, c: &Value<E>) -> Result<Self> {
        Ok(match op {
            Op::Plus => Judgement::PlusIs(PlusIsJudgement(
                a.try_get_int()?,
                b.try_get_int()?,
                c.try_get_int()?,
            )),
            Op::Minus => Judgement::MinusIs(MinusIsJudgement(
                a.try_get_int()?,
                b.try_get_int()?,
                c.try_get_int()?,
            )),
            Op::Times => Judgement::TimesIs(TimesIsJudgement(
                a.try_get_int()?,
                b.try_get_int()?,
                c.try_get_int()?,
            )),
            Op::Lt => Judgement::LtIs(LtIsJudgement(
                a.try_get_int()?,
                b.try_get_int()?,
                c.try_get_bool()?,
            )),
        })
    }

    pub fn eval_result(&self) -> Result<&Value<E>> {
        match self {
            Judgement::EvalTo(e) => Some(&e.value),
            Judgement::PlusIs(PlusIsJudgement(_, _, _)) => None,
            Judgement::MinusIs(MinusIsJudgement(_, _, _)) => None,
            Judgement::TimesIs(TimesIsJudgement(_, _, _)) => None,
            Judgement::LtIs(LtIsJudgement(_, _, _)) => None,
        }
        .ok_or(Error::NotEvalToJudgement)
    }
}

impl<E: Env> Visitable for Judgement<E> {}

impl<E: Env> Visitable for EvalToJudgement<E> {}

impl Visitable for PlusIsJudgement {}

impl Visitable for MinusIsJudgement {}

impl Visitable for TimesIsJudgement {}

impl Visitable for LtIsJudgement {}
