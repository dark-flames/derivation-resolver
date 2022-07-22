use crate::print::{ToToken, TokenBuffer};
use crate::systems::common::syntax::*;
use crate::visitor::AstNode;
use std::fmt::{Result as FmtResult, Write};

pub type BoxedNode = Box<EvalML3Node>;

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

impl AstNode for EvalML3Node {}

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

impl ToToken for EvalML3Node {
    fn to_token(&self, buffer: &mut TokenBuffer) -> FmtResult {
        match self {
            EvalML3Node::Integer(IntegerNode(i)) => write!(buffer, "{}", i),
            EvalML3Node::Boolean(BooleanNode(b)) => write!(buffer, "{}", b),
            EvalML3Node::Variable(VariableNode(v)) => write!(buffer, "{}", v),
            EvalML3Node::OpTerm(op_node) => op_node.to_token(buffer),
            EvalML3Node::IfTerm(if_node) => if_node.to_token(buffer),
            EvalML3Node::LetInTerm(let_in_node) => let_in_node.to_token(buffer),
            EvalML3Node::FunctionTerm(function_node) => function_node.to_token(buffer),
            EvalML3Node::ApplicationTerm(ap_node) => ap_node.to_token(buffer),
            EvalML3Node::LetRecInTerm(let_rec_in_node) => let_rec_in_node.to_token(buffer),
        }
    }
}
