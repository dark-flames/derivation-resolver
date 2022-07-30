use crate::derive::{DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::derive::{DerivationTreeOf, DeriveVisitor};
use crate::systems::common::env::{Env, NamedEnv};
use crate::systems::common::judgement::{EvalToJudgement, Judgement};
use crate::systems::common::syntax::{AstRoot, VariableNode};
use crate::systems::common::value::{ConcatList, Value};
use crate::systems::eval_ml_4::rules::*;
use crate::systems::eval_ml_4::syntax::{EvalML4Node, ListPatternMatchNode};
use crate::visitor::Visitor;
use crate::Visitable;

impl Visitor<VariableNode, Result<DerivationTreeOf<NamedEnv<EvalML4Node>>>>
    for DeriveVisitor<NamedEnv<EvalML4Node>>
{
    fn visit(&mut self, node: &VariableNode) -> Result<DerivationTreeOf<NamedEnv<EvalML4Node>>> {
        let env = &self.env;
        self.env
            .lookup_named(
                |id, value, _| {
                    (id.is_some() && id.unwrap() == node.0.as_str()).then(|| {
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
                },
                |r, _| r,
            )
            .ok_or(Error::UnknownIdentifier)?
    }
}

impl Visitor<ListPatternMatchNode<EvalML4Node>, Result<DerivationTreeOf<NamedEnv<EvalML4Node>>>>
    for DeriveVisitor<NamedEnv<EvalML4Node>>
{
    fn visit(
        &mut self,
        node: &ListPatternMatchNode<EvalML4Node>,
    ) -> Result<DerivationTreeOf<NamedEnv<EvalML4Node>>> {
        let expr_tree = node.expr.apply_visitor(self)?;

        match expr_tree.judgement.eval_result()? {
            Value::NilList => {
                let body_tree = node.nil_pattern.apply_visitor(self)?;

                Ok(DerivationTree::new(
                    Judgement::EvalTo(EvalToJudgement::new(
                        self.env.clone(),
                        EvalML4Node::ListPatternMatchTerm(node.clone()),
                        body_tree.judgement.eval_result()?.clone(),
                    )),
                    E_MATCH_NIL,
                    vec![expr_tree, body_tree],
                ))
            }
            Value::ConcatList(ConcatList { box lhs, box rhs }) => {
                let mut new_visitor = DeriveVisitor::new(
                    self.env
                        .clone()
                        .append_named(node.head_id.clone(), lhs.clone())
                        .append_named(node.tail_id.clone(), rhs.clone()),
                );

                let body_tree = node.list_pattern.apply_visitor(&mut new_visitor)?;

                Ok(DerivationTree::new(
                    Judgement::EvalTo(EvalToJudgement::new(
                        self.env.clone(),
                        EvalML4Node::ListPatternMatchTerm(node.clone()),
                        body_tree.judgement.eval_result()?.clone(),
                    )),
                    E_MATCH_CONS,
                    vec![expr_tree, body_tree],
                ))
            }
            _ => Err(Error::UnsupportedPattern),
        }
    }
}

impl Visitor<EvalML4Node, Result<DerivationTreeOf<NamedEnv<EvalML4Node>>>>
    for DeriveVisitor<NamedEnv<EvalML4Node>>
{
    fn visit(&mut self, node: &EvalML4Node) -> Result<DerivationTreeOf<NamedEnv<EvalML4Node>>> {
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
