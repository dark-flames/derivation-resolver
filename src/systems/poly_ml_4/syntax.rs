use crate::derive::Result;
use crate::systems::common::syntax::*;
use crate::visitor::Visitor;
use crate::{PrintVisitor, Visitable};
use std::fmt::Result as FmtResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyML4Node {
    Integer(IntegerNode),
    Boolean(BooleanNode),
    Variable(VariableNode),
    OpTerm(OpNode<PolyML4Node>),
    IfTerm(IfNode<PolyML4Node>),
    LetInTerm(LetInNode<PolyML4Node>),
    FunctionTerm(FunctionNode<PolyML4Node>),
    ApplicationTerm(ApplicationNode<PolyML4Node>),
    LetRecInTerm(LetRecInNode<PolyML4Node>),
    NilListTerm(NilListNode),
    ListConcatTerm(ListConcatNode<PolyML4Node>),
    ListPatternMatchTerm(ListPatternMatchNode<PolyML4Node>),
}

impl Visitable for PolyML4Node {}

impl AsOpNums for PolyML4Node {
    fn need_paren(&self, op: Op, left: bool) -> bool {
        match self {
            PolyML4Node::Integer(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::Boolean(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::Variable(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::OpTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::IfTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::LetInTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::FunctionTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::ApplicationTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::LetRecInTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::NilListTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::ListConcatTerm(n) => AsOpNums::need_paren(n, op, left),
            PolyML4Node::ListPatternMatchTerm(n) => AsOpNums::need_paren(n, op, left),
        }
    }
}

impl AsParam for PolyML4Node {
    fn need_paren(&self) -> bool {
        match self {
            PolyML4Node::Integer(n) => AsParam::need_paren(n),
            PolyML4Node::Boolean(n) => AsParam::need_paren(n),
            PolyML4Node::Variable(n) => AsParam::need_paren(n),
            PolyML4Node::OpTerm(n) => AsParam::need_paren(n),
            PolyML4Node::IfTerm(n) => AsParam::need_paren(n),
            PolyML4Node::LetInTerm(n) => AsParam::need_paren(n),
            PolyML4Node::FunctionTerm(n) => AsParam::need_paren(n),
            PolyML4Node::ApplicationTerm(n) => AsParam::need_paren(n),
            PolyML4Node::LetRecInTerm(n) => AsParam::need_paren(n),
            PolyML4Node::NilListTerm(n) => AsParam::need_paren(n),
            PolyML4Node::ListConcatTerm(n) => AsParam::need_paren(n),
            PolyML4Node::ListPatternMatchTerm(n) => AsParam::need_paren(n),
        }
    }
}

impl AsListSeg for PolyML4Node {
    fn need_paren(&self, left: bool) -> bool {
        match self {
            PolyML4Node::Integer(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::Boolean(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::Variable(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::OpTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::IfTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::LetInTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::FunctionTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::ApplicationTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::LetRecInTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::NilListTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::ListConcatTerm(n) => AsListSeg::need_paren(n, left),
            PolyML4Node::ListPatternMatchTerm(n) => AsListSeg::need_paren(n, left),
        }
    }
}

impl Visitor<PolyML4Node, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &PolyML4Node) -> FmtResult {
        match node {
            PolyML4Node::Integer(n) => n.apply_visitor(self),
            PolyML4Node::Boolean(n) => n.apply_visitor(self),
            PolyML4Node::Variable(n) => n.apply_visitor(self),
            PolyML4Node::OpTerm(n) => n.apply_visitor(self),
            PolyML4Node::IfTerm(n) => n.apply_visitor(self),
            PolyML4Node::LetInTerm(n) => n.apply_visitor(self),
            PolyML4Node::FunctionTerm(n) => n.apply_visitor(self),
            PolyML4Node::ApplicationTerm(n) => n.apply_visitor(self),
            PolyML4Node::LetRecInTerm(n) => n.apply_visitor(self),
            PolyML4Node::NilListTerm(n) => n.apply_visitor(self),
            PolyML4Node::ListConcatTerm(n) => n.apply_visitor(self),
            PolyML4Node::ListPatternMatchTerm(n) => n.apply_visitor(self),
        }
    }
}

impl AstRoot for PolyML4Node {
    fn integer(value: i64) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::Integer(IntegerNode(value)))
    }

    fn boolean(value: bool) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::Boolean(BooleanNode(value)))
    }

    fn variable(name: String) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::Variable(VariableNode(name)))
    }

    fn op_term(node: OpNode<Self>) -> Result<Self>
    where
        Self: Sized + AsOpNums,
    {
        Ok(PolyML4Node::OpTerm(node))
    }

    fn if_term(node: IfNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::IfTerm(node))
    }

    fn let_in_term(node: LetInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::LetInTerm(node))
    }

    fn function_term(node: FunctionNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::FunctionTerm(node))
    }

    fn application_term(node: ApplicationNode<Self>) -> Result<Self>
    where
        Self: Sized + AsParam,
    {
        Ok(PolyML4Node::ApplicationTerm(node))
    }

    fn let_rec_in_term(node: LetRecInNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::LetRecInTerm(node))
    }

    fn nil_list() -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::NilListTerm(NilListNode))
    }

    fn list_concat(node: ListConcatNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::ListConcatTerm(node))
    }

    fn list_pattern_match(node: ListPatternMatchNode<Self>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(PolyML4Node::ListPatternMatchTerm(node))
    }
}
