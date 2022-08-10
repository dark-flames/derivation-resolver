use std::fmt::{Debug, Result as FmtResult};

use crate::derive::Result;
use crate::systems::common::print::PrintVisitor;
use crate::systems::common::syntax::{
    ApplicationNode, AsListSeg, AsOpNums, AsParam, AstRoot, BooleanNode, FunctionNode, IfNode,
    IntegerNode, LetInNode, LetRecInNode, ListConcatNode, ListPatternMatchNode, NilListNode, Op,
    OpNode, VariableNode,
};
use crate::visitor::{Visitable, Visitor};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalML4Node {
    Integer(IntegerNode),
    Boolean(BooleanNode),
    Variable(VariableNode),
    OpTerm(OpNode<EvalML4Node>),
    IfTerm(IfNode<EvalML4Node>),
    LetInTerm(LetInNode<EvalML4Node>),
    FunctionTerm(FunctionNode<EvalML4Node>),
    ApplicationTerm(ApplicationNode<EvalML4Node>),
    LetRecInTerm(LetRecInNode<EvalML4Node>),
    NilListTerm(NilListNode),
    ListConcatTerm(ListConcatNode<EvalML4Node>),
    ListPatternMatchTerm(ListPatternMatchNode<EvalML4Node>),
}

impl Visitable for EvalML4Node {}

impl AsOpNums for EvalML4Node {
    fn need_paren(&self, op: Op, left: bool) -> bool {
        match self {
            EvalML4Node::Integer(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::Boolean(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::Variable(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::OpTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::IfTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::LetInTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::FunctionTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::ApplicationTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::LetRecInTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::NilListTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::ListConcatTerm(n) => AsOpNums::need_paren(n, op, left),
            EvalML4Node::ListPatternMatchTerm(n) => AsOpNums::need_paren(n, op, left),
        }
    }
}

impl AsParam for EvalML4Node {
    fn need_paren(&self) -> bool {
        match self {
            EvalML4Node::Integer(n) => AsParam::need_paren(n),
            EvalML4Node::Boolean(n) => AsParam::need_paren(n),
            EvalML4Node::Variable(n) => AsParam::need_paren(n),
            EvalML4Node::OpTerm(n) => AsParam::need_paren(n),
            EvalML4Node::IfTerm(n) => AsParam::need_paren(n),
            EvalML4Node::LetInTerm(n) => AsParam::need_paren(n),
            EvalML4Node::FunctionTerm(n) => AsParam::need_paren(n),
            EvalML4Node::ApplicationTerm(n) => AsParam::need_paren(n),
            EvalML4Node::LetRecInTerm(n) => AsParam::need_paren(n),
            EvalML4Node::NilListTerm(n) => AsParam::need_paren(n),
            EvalML4Node::ListConcatTerm(n) => AsParam::need_paren(n),
            EvalML4Node::ListPatternMatchTerm(n) => AsParam::need_paren(n),
        }
    }
}

impl AsListSeg for EvalML4Node {
    fn need_paren(&self, left: bool) -> bool {
        match self {
            EvalML4Node::Integer(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::Boolean(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::Variable(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::OpTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::IfTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::LetInTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::FunctionTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::ApplicationTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::LetRecInTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::NilListTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::ListConcatTerm(n) => AsListSeg::need_paren(n, left),
            EvalML4Node::ListPatternMatchTerm(n) => AsListSeg::need_paren(n, left),
        }
    }
}

impl AstRoot for EvalML4Node {
    fn integer(value: i64) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::Integer(IntegerNode(value)))
    }

    fn boolean(value: bool) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::Boolean(BooleanNode(value)))
    }

    fn variable(name: String) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::Variable(VariableNode(name)))
    }

    fn op_term(node: OpNode<Self>) -> Result<Self>
    where
        Self: Sized + AsOpNums,
    {
        Ok(EvalML4Node::OpTerm(node))
    }

    fn if_term(node: IfNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::IfTerm(node))
    }

    fn let_in_term(node: LetInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::LetInTerm(node))
    }

    fn function_term(node: FunctionNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::FunctionTerm(node))
    }

    fn application_term(node: ApplicationNode<Self>) -> Result<Self>
    where
        Self: Sized + AsParam,
    {
        Ok(EvalML4Node::ApplicationTerm(node))
    }

    fn let_rec_in_term(node: LetRecInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::LetRecInTerm(node))
    }

    fn nil_list() -> Result<Self> {
        Ok(EvalML4Node::NilListTerm(NilListNode))
    }

    fn list_concat(node: ListConcatNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::ListConcatTerm(node))
    }

    fn list_pattern_match(node: ListPatternMatchNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML4Node::ListPatternMatchTerm(node))
    }
}

impl Visitor<EvalML4Node, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &EvalML4Node) -> FmtResult {
        match node {
            EvalML4Node::Integer(i) => i.apply_visitor(self),
            EvalML4Node::Boolean(b) => b.apply_visitor(self),
            EvalML4Node::Variable(v) => v.apply_visitor(self),
            EvalML4Node::OpTerm(n) => n.apply_visitor(self),
            EvalML4Node::IfTerm(n) => n.apply_visitor(self),
            EvalML4Node::LetInTerm(n) => n.apply_visitor(self),
            EvalML4Node::FunctionTerm(n) => n.apply_visitor(self),
            EvalML4Node::ApplicationTerm(n) => n.apply_visitor(self),
            EvalML4Node::LetRecInTerm(n) => n.apply_visitor(self),
            EvalML4Node::NilListTerm(n) => n.apply_visitor(self),
            EvalML4Node::ListConcatTerm(n) => n.apply_visitor(self),
            EvalML4Node::ListPatternMatchTerm(n) => n.apply_visitor(self),
        }
    }
}
