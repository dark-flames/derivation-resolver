use crate::derive::{Derivable, DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::env::Env;
use crate::systems::common::judgement::{EvalToJudgement, Judgement};
use crate::systems::common::rules::*;
use crate::systems::common::syntax::{
    ApplicationNode, AsOpNums, AsParam, AstRoot, BooleanNode, FunctionNode, IfNode, IntegerNode,
    LetInNode, LetRecInNode, ListConcatNode, NilListNode, Op, OpNode,
};
use crate::systems::common::value::{ConcatList, Function, RecursiveFunction, Value};
use crate::visitor::Visitor;
use crate::Visitable;

pub type DerivationTreeOf<E> = DerivationTree<Judgement<E>>;

pub struct DeriveVisitor<E: Env> {
    pub(crate) env: E,
}

impl<E: Env> DeriveVisitor<E> {
    pub fn new(env: E) -> Self {
        DeriveVisitor { env }
    }
}

impl<E: Env> Visitor<IntegerNode, Result<DerivationTreeOf<E>>> for DeriveVisitor<E> {
    fn visit(&mut self, node: &IntegerNode) -> Result<DerivationTreeOf<E>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::integer(node.0)?,
                Value::Integer(node.0),
            )),
            E_INT,
            vec![],
        ))
    }
}

impl<E: Env> Visitor<BooleanNode, Result<DerivationTreeOf<E>>> for DeriveVisitor<E> {
    fn visit(&mut self, node: &BooleanNode) -> Result<DerivationTreeOf<E>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::boolean(node.0)?,
                Value::Boolean(node.0),
            )),
            E_BOOL,
            vec![],
        ))
    }
}

impl<E: Env> Visitor<OpNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    E::Ast: AsOpNums,
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
    Judgement<E>: Derivable,
{
    fn visit(&mut self, node: &OpNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let lhs_tree = node.lhs.apply_visitor(self)?;
        let rhs_tree = node.rhs.apply_visitor(self)?;
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
                E::Ast::op_term(node.clone())?,
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

impl<E: Env> Visitor<IfNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &IfNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let cond_tree = node.cond.apply_visitor(self)?;
        let (expr_tree, rule) = match cond_tree.judgement.eval_result()? {
            Value::Boolean(true) => node.t_branch.apply_visitor(self).map(|r| (r, E_IF_T)),
            Value::Boolean(false) => node.f_branch.apply_visitor(self).map(|r| (r, E_IF_F)),
            _ => Err(Error::NonBooleanValue),
        }?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::if_term(node.clone())?,
                expr_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![cond_tree, expr_tree],
        ))
    }
}

impl<E: Env> Visitor<LetInNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &LetInNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let tree_1 = node.expr_1.apply_visitor(self)?;
        let value_1 = tree_1.judgement.eval_result()?;
        let mut sub_visitor = DeriveVisitor::new(
            self.env
                .clone()
                .append_named(node.ident.clone(), value_1.clone()),
        );
        let tree_2 = node.expr_2.apply_visitor(&mut sub_visitor)?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::let_in_term(node.clone())?,
                tree_2.judgement.eval_result()?.clone(),
            )),
            E_LET,
            vec![tree_1, tree_2],
        ))
    }
}

impl<E: Env> Visitor<FunctionNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &FunctionNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::function_term(node.clone())?,
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

impl<E: Env> Visitor<LetRecInNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &LetRecInNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let mut new_visitor = DeriveVisitor::new(self.env.clone().append_named(
            node.ident.clone(),
            Value::RecFun(Box::new(RecursiveFunction {
                env: self.env.clone(),
                ident: node.ident.clone(),
                bind: node.bind.clone(),
                body: node.body.clone(),
            })),
        ));

        let expr_tree = node.expr.apply_visitor(&mut new_visitor)?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::let_rec_in_term(node.clone())?,
                expr_tree.judgement.eval_result()?.clone(),
            )),
            E_LET_REC,
            vec![expr_tree],
        ))
    }
}

impl<E: Env> Visitor<ApplicationNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    E::Ast: AsParam,
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &ApplicationNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let f_tree = node.f.apply_visitor(self)?;
        let p_tree = node.p.apply_visitor(self)?;
        let (new_env, body, rule) = match f_tree.judgement.eval_result()? {
            Value::Fun(box Function {
                env: f_env,
                bind,
                body,
            }) => Ok((
                f_env
                    .clone()
                    .append_named(bind.clone(), p_tree.judgement.eval_result()?.clone()),
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
                    .append_named(ident.clone(), f_tree.judgement.eval_result()?.clone())
                    .append_named(bind.clone(), p_tree.judgement.eval_result()?.clone()),
                body,
                E_APP_REC,
            )),
            _ => Err(Error::ApplyOnNonFunctionValue),
        }?;
        let mut new_visitor = DeriveVisitor::new(new_env);
        let app_tree = body.apply_visitor(&mut new_visitor)?;
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::application_term(node.clone())?,
                app_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![f_tree, p_tree, app_tree],
        ))
    }
}

impl<E: Env> Visitor<NilListNode, Result<DerivationTreeOf<E>>> for DeriveVisitor<E> {
    fn visit(&mut self, _node: &NilListNode) -> Result<DerivationTreeOf<E>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::nil_list()?,
                Value::NilList,
            )),
            E_NIL,
            vec![],
        ))
    }
}

impl<E: Env> Visitor<ListConcatNode<E::Ast>, Result<DerivationTreeOf<E>>> for DeriveVisitor<E>
where
    Self: Visitor<E::Ast, Result<DerivationTreeOf<E>>>,
{
    fn visit(&mut self, node: &ListConcatNode<E::Ast>) -> Result<DerivationTreeOf<E>> {
        let left_tree = node.lhs.apply_visitor(self)?;
        let left_result = left_tree.judgement.eval_result()?;
        let right_tree = node.rhs.apply_visitor(self)?;
        let right_result = right_tree.judgement.eval_result()?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                E::Ast::list_concat(node.clone())?,
                Value::ConcatList(ConcatList {
                    lhs: Box::new(left_result.clone()),
                    rhs: Box::new(right_result.clone()),
                }),
            )),
            E_CONS,
            vec![left_tree, right_tree],
        ))
    }
}
