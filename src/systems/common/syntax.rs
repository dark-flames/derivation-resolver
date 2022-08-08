use std::cmp::Ordering;

use crate::derive::Result;
use crate::error::Error;
use crate::systems::common::env::{Env, TypedEnv};
use crate::systems::common::value::Value;
use crate::visitor::Visitable;

pub trait AsOpNums: Visitable {
    fn need_paren(&self, _op: Op, _left: bool) -> bool {
        false
    }
}

pub trait AsParam: Visitable {
    fn need_paren(&self) -> bool {
        false
    }
}

pub trait AsListSeg: Visitable {
    fn need_paren(&self, _left: bool) -> bool {
        false
    }
}
pub trait AsTypedTerm<Ast: AstRoot> {
    fn need_hint(&self, _env: &TypedEnv<Ast>) -> bool {
        true
    }
}

pub trait AstRoot: Visitable {
    fn integer(_value: i64) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn boolean(_value: bool) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn variable(_name: String) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn op_term(_node: OpNode<Self>) -> Result<Self>
    where
        Self: Sized + AsOpNums,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn if_term(_node: IfNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn let_in_term(_node: LetInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn function_term(_node: FunctionNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn application_term(_node: ApplicationNode<Self>) -> Result<Self>
    where
        Self: Sized + AsParam,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn let_rec_in_term(_node: LetRecInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn nil_list() -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn list_concat(_node: ListConcatNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
    }

    fn list_pattern_match(_node: ListPatternMatchNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Err(Error::UnsupportedSyntax)
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
pub struct OpNode<Node: AstRoot + AsOpNums> {
    pub lhs: Box<Node>,
    pub op: Op,
    pub rhs: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfNode<Node: AstRoot> {
    pub cond: Box<Node>,
    pub t_branch: Box<Node>,
    pub f_branch: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LetInNode<Node: AstRoot> {
    pub ident: Ident,
    pub expr_1: Box<Node>,
    pub expr_2: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FunctionNode<Node: AstRoot> {
    pub bind: Ident,
    pub body: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ApplicationNode<Node: AstRoot + AsParam> {
    pub f: Box<Node>,
    pub p: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LetRecInNode<Node: AstRoot> {
    pub ident: Ident,
    pub bind: Ident,
    pub body: Box<Node>,
    pub expr: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct NilListNode;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ListConcatNode<Node: AstRoot> {
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ListPatternMatchNode<Node: AstRoot> {
    pub expr: Box<Node>,
    pub nil_branch: Box<Node>,
    pub head_id: Ident,
    pub tail_id: Ident,
    pub list_branch: Box<Node>,
}

impl Visitable for IntegerNode {}
impl AsOpNums for IntegerNode {}
impl AsParam for IntegerNode {}
impl AsListSeg for IntegerNode {}

impl Visitable for BooleanNode {}
impl AsOpNums for BooleanNode {}
impl AsParam for BooleanNode {}
impl AsListSeg for BooleanNode {}

impl Visitable for VariableNode {}
impl AsOpNums for VariableNode {}
impl AsParam for VariableNode {}
impl AsListSeg for VariableNode {}

impl<Node: AstRoot + AsOpNums> Visitable for OpNode<Node> {}
impl<Node: AstRoot + AsOpNums> AsOpNums for OpNode<Node> {
    fn need_paren(&self, op: Op, _left: bool) -> bool {
        self.op < op
    }
}
impl<Node: AstRoot + AsOpNums> AsParam for OpNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}
impl<Node: AstRoot + AsOpNums> AsListSeg for OpNode<Node> {}

impl<Node: AstRoot> Visitable for IfNode<Node> {}
impl<Node: AstRoot> AsOpNums for IfNode<Node> {
    fn need_paren(&self, _op: Op, left: bool) -> bool {
        left
    }
}
impl<Node: AstRoot> AsParam for IfNode<Node> {}
impl<Node: AstRoot> AsListSeg for IfNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}

impl<Node: AstRoot> AsOpNums for LetInNode<Node> {
    fn need_paren(&self, _op: Op, left: bool) -> bool {
        left
    }
}
impl<Node: AstRoot> AsParam for LetInNode<Node> {}
impl<Node: AstRoot> Visitable for LetInNode<Node> {}
impl<Node: AstRoot> AsListSeg for LetInNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}

impl<Node: AstRoot> Visitable for LetRecInNode<Node> {}
impl<Node: AstRoot> AsOpNums for LetRecInNode<Node> {
    fn need_paren(&self, _op: Op, left: bool) -> bool {
        left
    }
}
impl<Node: AstRoot> AsParam for LetRecInNode<Node> {}
impl<Node: AstRoot> AsListSeg for LetRecInNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}

impl<Node: AstRoot> Visitable for FunctionNode<Node> {}
impl<Node: AstRoot> AsOpNums for FunctionNode<Node> {
    fn need_paren(&self, _op: Op, _left: bool) -> bool {
        true
    }
}
impl<Node: AstRoot> AsParam for FunctionNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}
impl<Node: AstRoot> AsListSeg for FunctionNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}

impl<Node: AstRoot + AsParam> AsOpNums for ApplicationNode<Node> {
    fn need_paren(&self, _op: Op, _left: bool) -> bool {
        true
    }
}
impl<Node: AstRoot + AsParam> AsParam for ApplicationNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}
impl<Node: AstRoot + AsParam> Visitable for ApplicationNode<Node> {}
impl<Node: AstRoot + AsParam> AsListSeg for ApplicationNode<Node> {
    fn need_paren(&self, _left: bool) -> bool {
        true
    }
}

impl Visitable for NilListNode {}
impl AsOpNums for NilListNode {}
impl AsParam for NilListNode {}
impl AsListSeg for NilListNode {}

impl<Node: AstRoot> Visitable for ListConcatNode<Node> {}
impl<Node: AstRoot> AsOpNums for ListConcatNode<Node> {
    fn need_paren(&self, _op: Op, _left: bool) -> bool {
        true
    }
}
impl<Node: AstRoot> AsParam for ListConcatNode<Node> {
    fn need_paren(&self) -> bool {
        true
    }
}
impl<Node: AstRoot> AsListSeg for ListConcatNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}

impl Op {
    pub fn apply<E: Env>(&self, lhs: &Value<E>, rhs: &Value<E>) -> Result<Value<E>> {
        Ok(match self {
            Op::Plus => Value::Integer(lhs.try_get_int()? + rhs.try_get_int()?),
            Op::Minus => Value::Integer(lhs.try_get_int()? - rhs.try_get_int()?),
            Op::Times => Value::Integer(lhs.try_get_int()? * rhs.try_get_int()?),
            Op::Lt => Value::Boolean(lhs.try_get_int()? < rhs.try_get_int()?),
        })
    }
}
impl PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        fn to_precedence(op: &Op) -> usize {
            match op {
                Op::Times => 2,
                Op::Plus => 1,
                Op::Minus => 1,
                Op::Lt => 0,
            }
        }

        let rhs = to_precedence(other);

        to_precedence(self).partial_cmp(&rhs)
    }
}
impl Visitable for Op {}

pub type Ident = String;

impl<Node: AstRoot> Visitable for ListPatternMatchNode<Node> {}

impl<Node: AstRoot> AsOpNums for ListPatternMatchNode<Node> {
    fn need_paren(&self, _op: Op, left: bool) -> bool {
        left
    }
}

impl<Node: AstRoot> AsParam for ListPatternMatchNode<Node> {}

impl<Node: AstRoot> AsListSeg for ListPatternMatchNode<Node> {
    fn need_paren(&self, left: bool) -> bool {
        left
    }
}
