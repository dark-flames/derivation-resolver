use crate::derive::{DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::env::Env;
use crate::systems::common::eval::{DerivationTreeOf, UntypedDeriveVisitor};
use crate::systems::common::judgement::{EvalToJudgement, Judgement};
use crate::systems::common::syntax::{AstRoot, VariableNode};
use crate::systems::eval_ml_4::rules::*;
use crate::systems::eval_ml_4::syntax::EvalML4Node;
use crate::visitor::{Visitable, Visitor};

impl Visitor<VariableNode, Result<DerivationTreeOf<EvalML4Node>>>
    for UntypedDeriveVisitor<EvalML4Node>
{
    fn visit(&mut self, node: &VariableNode) -> Result<DerivationTreeOf<EvalML4Node>> {
        let env = &self.env;
        self.env
            .look_up(&node.0)
            .ok_or_else(|| Error::UnknownIdentifier(node.0.clone()))
            .and_then(|value| {
                Ok(DerivationTree::new(
                    Judgement::EvalTo(EvalToJudgement::new(
                        env.clone(),
                        EvalML4Node::variable(node.0.clone())?,
                        value.clone(),
                    )),
                    E_VAR,
                    vec![],
                ))
            })
    }
}

impl Visitor<EvalML4Node, Result<DerivationTreeOf<EvalML4Node>>>
    for UntypedDeriveVisitor<EvalML4Node>
{
    fn visit(&mut self, node: &EvalML4Node) -> Result<DerivationTreeOf<EvalML4Node>> {
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
