use crate::systems::common::print::PrintVisitor;
use crate::systems::common::syntax::AstRoot;
use crate::systems::common::syntax::*;
use crate::visitor::{Visitable, Visitor};
use std::fmt::{Result as FmtResult, Write};

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum EvalML3Node {
    Integer(IntegerNode),
    Boolean(BooleanNode),
    Variable(VariableNode),
    OpTerm(OpNode<EvalML3Node>),
    IfTerm(IfNode<EvalML3Node>),
    LetInTerm(LetInNode<EvalML3Node>),
    FunctionTerm(FunctionNode<EvalML3Node>),
    ApplicationTerm(ApplicationNode<EvalML3Node>),
    LetRecInTerm(LetRecInNode<EvalML3Node>),
}

impl Visitable for EvalML3Node {}

impl AsOpNums for EvalML3Node {
    fn need_paren(&self, op: Op) -> bool {
        match self {
            EvalML3Node::Integer(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::Boolean(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::Variable(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::OpTerm(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::IfTerm(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::LetInTerm(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::FunctionTerm(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::ApplicationTerm(n) => AsOpNums::need_paren(n, op),
            EvalML3Node::LetRecInTerm(n) => AsOpNums::need_paren(n, op),
        }
    }
}

impl AsParam for EvalML3Node {
    fn need_paren(&self) -> bool {
        match self {
            EvalML3Node::Integer(n) => AsParam::need_paren(n),
            EvalML3Node::Boolean(n) => AsParam::need_paren(n),
            EvalML3Node::Variable(n) => AsParam::need_paren(n),
            EvalML3Node::OpTerm(n) => AsParam::need_paren(n),
            EvalML3Node::IfTerm(n) => AsParam::need_paren(n),
            EvalML3Node::LetInTerm(n) => AsParam::need_paren(n),
            EvalML3Node::FunctionTerm(n) => AsParam::need_paren(n),
            EvalML3Node::ApplicationTerm(n) => AsParam::need_paren(n),
            EvalML3Node::LetRecInTerm(n) => AsParam::need_paren(n),
        }
    }
}

impl Visitor<EvalML3Node, FmtResult> for PrintVisitor {
    fn visit(&mut self, node: &EvalML3Node) -> FmtResult {
        match node {
            EvalML3Node::Integer(IntegerNode(i)) => write!(self.buffer, "{}", i),
            EvalML3Node::Boolean(BooleanNode(b)) => write!(self.buffer, "{}", b),
            EvalML3Node::Variable(VariableNode(v)) => write!(self.buffer, "{}", v),
            EvalML3Node::OpTerm(op_node) => op_node.apply_visitor(self),
            EvalML3Node::IfTerm(if_node) => if_node.apply_visitor(self),
            EvalML3Node::LetInTerm(let_in_node) => let_in_node.apply_visitor(self),
            EvalML3Node::FunctionTerm(function_node) => function_node.apply_visitor(self),
            EvalML3Node::ApplicationTerm(ap_node) => ap_node.apply_visitor(self),
            EvalML3Node::LetRecInTerm(let_rec_in_node) => let_rec_in_node.apply_visitor(self),
        }
    }
}

impl AstRoot for EvalML3Node {
    fn integer(value: i64) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::Integer(IntegerNode(value)))
    }

    fn boolean(value: bool) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::Boolean(BooleanNode(value)))
    }

    fn variable(name: String) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::Variable(VariableNode(name)))
    }

    fn op_term(node: OpNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::OpTerm(node))
    }

    fn if_term(node: IfNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::IfTerm(node))
    }

    fn let_in_term(node: LetInNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::LetInTerm(node))
    }

    fn function_term(node: FunctionNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::FunctionTerm(node))
    }

    fn application_term(node: ApplicationNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::ApplicationTerm(node))
    }

    fn let_rec_in_term(node: LetRecInNode<Self>) -> crate::derive::Result<Self>
    where
        Self: Sized,
    {
        Ok(EvalML3Node::LetRecInTerm(node))
    }
}
