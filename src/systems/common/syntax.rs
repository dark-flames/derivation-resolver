use crate::print::TokenBuffer;
use crate::systems::common::value::Value;
use crate::systems::eval_ml_3::rules::{B_LT, B_MINUS, B_PLUS, B_TIMES};
use crate::visitor::AstNode;
use crate::{derive, ToToken};
use std::cmp::Ordering;
use std::fmt::{Result as FmtResult, Write};

pub trait AsOpNums: AstNode {
    fn need_paren(&self, _op: Op) -> bool {
        false
    }
}

pub trait AsParam: AstNode {
    fn need_paren(&self) -> bool {
        false
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IntegerNode(pub i64);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BooleanNode(pub bool);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VariableNode(pub Ident);

#[derive(Clone, Copy, Eq, PartialEq, Ord, Debug)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Lt,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct OpNode<Node: AstNode + AsOpNums> {
    pub lhs: Box<Node>,
    pub op: Op,
    pub rhs: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfNode<Node: AstNode> {
    pub cond: Box<Node>,
    pub t_branch: Box<Node>,
    pub f_branch: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LetInNode<Node: AstNode> {
    pub ident: Ident,
    pub expr_1: Box<Node>,
    pub expr_2: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FunctionNode<Node: AstNode> {
    pub bind: Ident,
    pub body: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ApplicationNode<Node: AstNode + AsParam> {
    pub f: Box<Node>,
    pub p: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LetRecInNode<Node: AstNode> {
    pub ident: Ident,
    pub bind: Ident,
    pub body: Box<Node>,
    pub expr: Box<Node>,
}

impl ToToken for IntegerNode {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "{}", self.0)
    }
}

impl AstNode for IntegerNode {}
impl AsOpNums for IntegerNode {}
impl AsParam for IntegerNode {}

impl ToToken for BooleanNode {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "{}", if self.0 { true } else { false })
    }
}

impl AstNode for BooleanNode {}
impl AsOpNums for BooleanNode {}
impl AsParam for BooleanNode {}

impl ToToken for VariableNode {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "{}", self.0)
    }
}

impl AstNode for VariableNode {}
impl AsOpNums for VariableNode {}
impl AsParam for VariableNode {}

impl<Node: AstNode + AsOpNums> ToToken for OpNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        if self.lhs.need_paren(self.op) {
            buffer.parenthesized(&self.lhs)?;
        } else {
            self.lhs.to_token(buffer)?;
        }

        self.op.to_token(buffer)?;
        if self.rhs.need_paren(self.op) {
            buffer.parenthesized(&self.rhs)
        } else {
            self.rhs.to_token(buffer)
        }
    }
}

impl<Node: AstNode + AsOpNums> AstNode for OpNode<Node> {}

impl<Node: AstNode + AsOpNums> AsOpNums for OpNode<Node> {
    fn need_paren(&self, op: Op) -> bool {
        self.op < op
    }
}

impl<Node: AstNode + AsOpNums> AsParam for OpNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}

impl<Node: AstNode> ToToken for IfNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        buffer.write_str("if")?;
        self.cond.to_token(buffer)?;
        buffer.write_str("then")?;
        self.t_branch.to_token(buffer)?;
        buffer.write_str("else")?;
        self.f_branch.to_token(buffer)
    }
}

impl<Node: AstNode> AstNode for IfNode<Node> {}

impl<Node: AstNode> AsOpNums for IfNode<Node> {}

impl<Node: AstNode> AsParam for IfNode<Node> {}

impl<Node: AstNode> AsOpNums for LetInNode<Node> {}

impl<Node: AstNode> AsParam for LetInNode<Node> {}

impl<Node: AstNode> ToToken for LetInNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "let {} =", self.ident)?;
        self.expr_1.to_token(buffer)?;
        buffer.write_str("in")?;
        self.expr_2.to_token(buffer)
    }
}

impl<Node: AstNode> AstNode for LetInNode<Node> {}

impl<Node: AstNode> ToToken for LetRecInNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "let rec {} = fun {} ->", self.ident, self.bind)?;
        buffer.parenthesized(&self.body)?;
        buffer.write_str("in")?;
        buffer.parenthesized(&self.expr)
    }
}

impl<Node: AstNode> AstNode for LetRecInNode<Node> {}

impl<Node: AstNode> AsOpNums for LetRecInNode<Node> {}

impl<Node: AstNode> AsParam for LetRecInNode<Node> {}

impl<Node: AstNode> ToToken for FunctionNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        write!(buffer, "fun {} ->", self.bind)?;
        self.body.to_token(buffer)
    }
}

impl<Node: AstNode> AstNode for FunctionNode<Node> {}

impl<Node: AstNode> AsOpNums for FunctionNode<Node> {}

impl<Node: AstNode> AsParam for FunctionNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}

impl<Node: AstNode + AsParam> ToToken for ApplicationNode<Node> {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        self.f.to_token(buffer)?;
        if self.p.need_paren() {
            buffer.parenthesized(&self.p)
        } else {
            self.p.to_token(buffer)
        }
    }
}

impl<Node: AstNode + AsParam> AsOpNums for ApplicationNode<Node> {
    fn need_paren(&self, _op: Op) -> bool {
        true
    }
}

impl<Node: AstNode + AsParam> AsParam for ApplicationNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}

impl<Node: AstNode + AsParam> AstNode for ApplicationNode<Node> {}

impl Op {
    pub fn apply(&self, lhs: &Value, rhs: &Value) -> derive::Result<Value> {
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

pub type Ident = String;
