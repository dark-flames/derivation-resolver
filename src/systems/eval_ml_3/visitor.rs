use crate::derive::Result;
use crate::derive::{Derivable, DerivationTree};
use crate::error::Error;
use crate::systems::common::env::{Env, NamedEnv};
use crate::systems::common::judgement::{EvalToJudgement, Judgement};
use crate::systems::common::syntax::{
    ApplicationNode, BooleanNode, FunctionNode, IfNode, IntegerNode, LetInNode, LetRecInNode, Op,
    OpNode, VariableNode,
};
use crate::systems::common::value::{Function, RecursiveFunction, Value};
use crate::systems::eval_ml_3::rules::*;
use crate::systems::eval_ml_3::syntax::EvalML3Node;
use crate::visitor::Visitor;

type EvalML3Judgement = Judgement<NamedEnv>;
type EvalML3DerivationTree = DerivationTree<EvalML3Judgement>;

pub struct DeriveVisitor<E: Env> {
    env: E,
}

impl<E: Env> DeriveVisitor<E> {
    pub fn new(env: E) -> Self {
        DeriveVisitor { env }
    }
}

impl Visitor<IntegerNode, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &IntegerNode) -> Result<EvalML3DerivationTree> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::Integer(node.clone()),
                Value::Integer(node.0),
            )),
            E_INT,
            vec![],
        ))
    }
}

impl Visitor<BooleanNode, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &BooleanNode) -> Result<EvalML3DerivationTree> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::Boolean(node.clone()),
                Value::Boolean(node.0),
            )),
            E_BOOL,
            vec![],
        ))
    }
}

impl Visitor<VariableNode, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &VariableNode) -> Result<EvalML3DerivationTree> {
        let id = &node.0;
        self.env
            .lookup(
                |(cur_id, value), env| {
                    (id == cur_id).then(|| {
                        DerivationTree::new(
                            Judgement::EvalTo(EvalToJudgement::new(
                                env.clone(),
                                EvalML3Node::Variable(node.clone()),
                                value.clone(),
                            )),
                            E_VAR1,
                            vec![],
                        )
                    })
                },
                |t, env| {
                    DerivationTree::new(
                        Judgement::EvalTo(EvalToJudgement::new(
                            env.clone(),
                            EvalML3Node::Variable(node.clone()),
                            t.judgement.eval_result().unwrap().clone(),
                        )),
                        E_VAR2,
                        vec![t],
                    )
                },
            )
            .ok_or_else(|| Error::UnknownIdentifier)
    }
}

impl Visitor<OpNode<EvalML3Node>, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &OpNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        let lhs_tree = self.visit(node.lhs.as_ref())?;
        let rhs_tree = self.visit(node.rhs.as_ref())?;
        let result = node.op.apply(
            lhs_tree.judgement.eval_result()?,
            rhs_tree.judgement.eval_result()?,
        )?;

        let b_tree = Judgement::from_op(
            node.op,
            lhs_tree.judgement.eval_result()?,
            rhs_tree.judgement.eval_result()?,
            &result,
        )?
        .derive()?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::OpTerm(node.clone()),
                result,
            )),
            match node.op {
                Op::Plus => E_PLUS,
                Op::Minus => E_MINUS,
                Op::Times => E_TIMES,
                Op::Lt => E_LT,
            },
            vec![lhs_tree, rhs_tree, b_tree],
        ))
    }
}

impl Visitor<IfNode<EvalML3Node>, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &IfNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        let cond_tree = self.visit(node.cond.as_ref())?;
        let (expr_tree, rule) = match cond_tree.judgement.eval_result()? {
            Value::Boolean(true) => self.visit(node.t_branch.as_ref()).map(|r| (r, E_IF_T)),
            Value::Boolean(false) => self.visit(node.f_branch.as_ref()).map(|r| (r, E_IF_F)),
            _ => Err(Error::NonBooleanValue),
        }?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::IfTerm(node.clone()),
                expr_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![cond_tree, expr_tree],
        ))
    }
}

impl Visitor<LetInNode<EvalML3Node>, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &LetInNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        let tree_1 = self.visit(node.expr_1.as_ref())?;
        let value_1 = tree_1.judgement.eval_result()?;
        let mut sub_visitor = DeriveVisitor::new(
            self.env
                .clone()
                .append((node.ident.clone(), value_1.clone())),
        );
        let tree_2 = sub_visitor.visit(node.expr_2.as_ref())?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::LetInTerm(node.clone()),
                tree_2.judgement.eval_result()?.clone(),
            )),
            E_LET,
            vec![tree_1, tree_2],
        ))
    }
}

impl Visitor<FunctionNode<EvalML3Node>, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &FunctionNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::FunctionTerm(node.clone()),
                Value::Fun(Box::new(Function {
                    env: self.env.clone(),
                    bind: node.bind.clone(),
                    body: node.body.clone(),
                })),
            )),
            E_FUN,
            vec![],
        ))
    }
}

impl Visitor<LetRecInNode<EvalML3Node>, Result<EvalML3DerivationTree>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &LetRecInNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        let mut new_visitor = DeriveVisitor::new(self.env.clone().append((
            node.ident.clone(),
            Value::RecFun(Box::new(RecursiveFunction {
                env: self.env.clone(),
                ident: node.ident.clone(),
                bind: node.bind.clone(),
                body: node.body.clone(),
            })),
        )));

        let expr_tree = new_visitor.visit(node.expr.as_ref())?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::LetRecInTerm(node.clone()),
                expr_tree.judgement.eval_result()?.clone(),
            )),
            E_LET_REC,
            vec![expr_tree],
        ))
    }
}

impl Visitor<ApplicationNode<EvalML3Node>, Result<EvalML3DerivationTree>>
    for DeriveVisitor<NamedEnv>
{
    fn visit(&mut self, node: &ApplicationNode<EvalML3Node>) -> Result<EvalML3DerivationTree> {
        let f_tree = self.visit(node.f.as_ref())?;
        let p_tree = self.visit(node.p.as_ref())?;
        let (new_env, body, rule) = match f_tree.judgement.eval_result()? {
            Value::Fun(box Function {
                env: f_env,
                bind,
                body,
            }) => Ok((
                f_env
                    .clone()
                    .append((bind.clone(), p_tree.judgement.eval_result()?.clone())),
                body,
                E_APP,
            )),
            Value::RecFun(box RecursiveFunction {
                env: f_env,
                ident,
                bind,
                body,
            }) => Ok((
                f_env
                    .clone()
                    .append((ident.clone(), f_tree.judgement.eval_result()?.clone()))
                    .append((bind.clone(), p_tree.judgement.eval_result()?.clone())),
                body,
                E_APP_REC,
            )),
            _ => Err(Error::ApplyOnNonFunctionValue),
        }?;
        let mut new_visitor = DeriveVisitor::new(new_env);
        let app_tree = new_visitor.visit(body.as_ref())?;
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                EvalML3Node::ApplicationTerm(node.clone()),
                app_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![f_tree, p_tree, app_tree],
        ))
    }
}

impl Visitor<EvalML3Node, Result<DerivationTree<Judgement<NamedEnv>>>> for DeriveVisitor<NamedEnv> {
    fn visit(&mut self, node: &EvalML3Node) -> Result<DerivationTree<Judgement<NamedEnv>>> {
        match node {
            EvalML3Node::Integer(n) => self.visit(n),
            EvalML3Node::Boolean(n) => self.visit(n),
            EvalML3Node::Variable(n) => self.visit(n),
            EvalML3Node::OpTerm(n) => self.visit(n),
            EvalML3Node::IfTerm(n) => self.visit(n),
            EvalML3Node::LetInTerm(n) => self.visit(n),
            EvalML3Node::FunctionTerm(n) => self.visit(n),
            EvalML3Node::ApplicationTerm(n) => self.visit(n),
            EvalML3Node::LetRecInTerm(n) => self.visit(n),
        }
    }
}
