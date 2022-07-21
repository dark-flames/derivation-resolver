use crate::error::Error;
use crate::interface::{Judgement as JudgementTrait, Result};
use crate::print::{ToToken, TokenBuffer};
use crate::systems::eval_ml_3::rules::*;
use crate::systems::eval_ml_3::syntax::Judgement::{LtIs, MinusIs, PlusIs, TimesIs};
use std::cmp::Ordering;
use std::fmt::{Result as FmtResult, Write};

pub type Ident = String;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Env {
    Terminal,
    Segment(Box<Env>, String, Value),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Function {
    pub env: Env,
    pub bind: Ident,
    pub body: BoxedNode,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct RecursiveFunction {
    pub env: Env,
    pub ident: Ident,
    pub bind: Ident,
    pub body: BoxedNode,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Fun(Box<Function>),
    RecFun(Box<RecursiveFunction>),
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, Debug)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Lt,
}

pub type BoxedNode = Box<AstNode>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum AstNode {
    Integer(i64),
    Boolean(bool),
    Variable(Ident),
    Op {
        lhs: BoxedNode,
        op: Op,
        rhs: BoxedNode,
    },
    IfTerm {
        cond: BoxedNode,
        t_branch: BoxedNode,
        f_branch: BoxedNode,
    },
    LetInTerm {
        ident: Ident,
        expr_1: BoxedNode,
        expr_2: BoxedNode,
    },
    Function {
        bind: Ident,
        body: BoxedNode,
    },
    Application {
        f: BoxedNode,
        p: BoxedNode,
    },
    LetRecIn {
        ident: Ident,
        bind: Ident,
        body: BoxedNode,
        expr: BoxedNode,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Judgement {
    EvalTo(EvalToJudgement),
    PlusIs(i64, i64, i64),
    MinusIs(i64, i64, i64),
    TimesIs(i64, i64, i64),
    LtIs(i64, i64, bool),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct EvalToJudgement {
    pub env: Env,
    pub term: AstNode,
    pub value: Value,
}

impl Env {
    pub fn collect(&self) -> Vec<(&Ident, &Value)> {
        match self {
            Env::Terminal => vec![],
            Env::Segment(next, id, value) => {
                let mut result = next.collect();

                result.push((id, value));

                result
            }
        }
    }

    pub fn append(self, id: Ident, value: Value) -> Self {
        Env::Segment(Box::new(self), id, value)
    }
}

impl Value {
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

impl Op {
    pub fn apply(&self, lhs: &Value, rhs: &Value) -> Result<Value> {
        Ok(match self {
            Op::Plus => Value::Integer(lhs.try_get_int()? + rhs.try_get_int()?),
            Op::Minus => Value::Integer(lhs.try_get_int()? - rhs.try_get_int()?),
            Op::Times => Value::Integer(lhs.try_get_int()? * rhs.try_get_int()?),
            Op::Lt => Value::Boolean(lhs.try_get_int()? < rhs.try_get_int()?),
        })
    }

    pub fn to_rule(&self) -> &'static str {
        match self {
            Op::Plus => B_PLUS,
            Op::Minus => B_MINUS,
            Op::Times => B_TIMES,
            Op::Lt => B_LT,
        }
    }
}

impl PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        fn to_precedence(op: &Op) -> usize {
            match op {
                Op::Plus => 1,
                Op::Minus => 1,
                Op::Times => 2,
                Op::Lt => 0,
            }
        }

        let rhs = to_precedence(other);

        to_precedence(self).partial_cmp(&rhs)
    }
}

impl JudgementTrait for Judgement {}

impl EvalToJudgement {
    pub fn new(env: Env, term: AstNode, value: Value) -> Self {
        EvalToJudgement { env, term, value }
    }
}

impl Judgement {
    pub fn from_op(op: Op, a: &Value, b: &Value, c: &Value) -> Result<Self> {
        Ok(match op {
            Op::Plus => PlusIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Minus => MinusIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Times => TimesIs(a.try_get_int()?, b.try_get_int()?, c.try_get_int()?),
            Op::Lt => LtIs(a.try_get_int()?, b.try_get_int()?, c.try_get_bool()?),
        })
    }
}

impl ToToken for Env {
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

impl ToToken for Function {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.parenthesized(&self.env)?;
        buffer.write_char('[')?;
        write!(buffer, "fun {} ->", self.bind)?;
        self.body.to_token(buffer)?;
        buffer.write_char(']')?;

        Ok(())
    }
}

impl ToToken for RecursiveFunction {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.parenthesized(&self.env)?;
        buffer.write_char('[')?;
        write!(buffer, "rec {} = fun {} ->", self.ident, self.bind)?;
        self.body.to_token(buffer)?;
        buffer.write_char(']')?;

        Ok(())
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

impl ToToken for Op {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.write_char(match self {
            Op::Plus => '+',
            Op::Minus => '-',
            Op::Times => '*',
            Op::Lt => '<',
        })
    }
}

impl ToToken for AstNode {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        match self {
            AstNode::Integer(i) => write!(buffer, "{}", i),
            AstNode::Boolean(b) => write!(buffer, "{}", b),
            AstNode::Variable(v) => write!(buffer, "{}", v),
            AstNode::Op { lhs, op, rhs } => {
                if matches!(lhs, box AstNode::Op {op: l_op, ..} if l_op < op)
                    || matches!(lhs, box AstNode::Application { .. })
                {
                    buffer.parenthesized(lhs)?;
                } else {
                    lhs.to_token(buffer)?;
                }

                op.to_token(buffer)?;
                if matches!(rhs, box AstNode::Op {op: l_op, ..} if l_op < op)
                    || matches!(rhs, box AstNode::Application { .. })
                {
                    buffer.parenthesized(rhs)
                } else {
                    rhs.to_token(buffer)
                }
            }
            AstNode::IfTerm {
                cond,
                t_branch,
                f_branch,
            } => {
                buffer.write_str("if")?;
                cond.to_token(buffer)?;
                buffer.write_str("then")?;
                t_branch.to_token(buffer)?;
                buffer.write_str("else")?;
                f_branch.to_token(buffer)
            }
            AstNode::LetInTerm {
                ident,
                expr_1,
                expr_2,
            } => {
                write!(buffer, "let {} =", ident)?;
                expr_1.to_token(buffer)?;
                buffer.write_str("in")?;
                expr_2.to_token(buffer)
            }
            AstNode::Function { bind, body } => {
                write!(buffer, "fun {} ->", bind)?;
                body.to_token(buffer)
            }
            AstNode::Application { f, p } => {
                f.to_token(buffer)?;
                if matches!(
                    p,
                    box AstNode::Application { .. }
                        | box AstNode::Function { .. }
                        | box AstNode::Op { .. }
                ) {
                    buffer.parenthesized(p)
                } else {
                    p.to_token(buffer)
                }
            }
            AstNode::LetRecIn {
                ident,
                bind,
                body,
                expr,
            } => {
                write!(buffer, "let rec {} = fun {} ->", ident, bind)?;
                buffer.parenthesized(body)?;
                buffer.write_str("in")?;
                buffer.parenthesized(expr)
            }
        }
    }
}

impl ToToken for EvalToJudgement {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        self.env.to_token(buffer)?;
        buffer.write_str("|-")?;
        self.term.to_token(buffer)?;
        buffer.write_str("evalto")?;
        self.value.to_token(buffer)
    }
}

impl ToToken for Judgement {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        match self {
            Judgement::EvalTo(eval_to) => eval_to.to_token(buffer),
            PlusIs(a, b, c) => write!(buffer, "{} plus {} is {}", a, b, c),
            MinusIs(a, b, c) => write!(buffer, "{} minus {} is {}", a, b, c),
            TimesIs(a, b, c) => write!(buffer, "{} times {} is {}", a, b, c),
            LtIs(a, b, c) => write!(buffer, "{} less than {} is {}", a, b, c),
        }
    }
}
