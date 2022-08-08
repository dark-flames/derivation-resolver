use crate::derive::{Derivable, DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::env::{Env, NamedEnv};
use crate::systems::common::judgement::{EvalToJudgement, Judgement};
use crate::systems::common::rules::*;
use crate::systems::common::syntax::{
    ApplicationNode, AsOpNums, AsParam, AstRoot, BooleanNode, FunctionNode, IfNode, IntegerNode,
    LetInNode, LetRecInNode, ListConcatNode, ListPatternMatchNode, NilListNode, Op, OpNode,
};
use crate::systems::common::value::{ConcatList, Function, RecursiveFunction, Value};
use crate::visitor::Visitor;

pub type DerivationTreeOf<Ast> = DerivationTree<Judgement<NamedEnv<Ast>>>;

pub struct UntypedDeriveVisitor<Ast: AstRoot> {
    pub(crate) env: NamedEnv<Ast>,
}

impl<Ast: AstRoot> UntypedDeriveVisitor<Ast> {
    pub fn new(env: NamedEnv<Ast>) -> Self {
        UntypedDeriveVisitor { env }
    }
}

impl<Ast: AstRoot> Visitor<IntegerNode, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
{
    fn visit(&mut self, node: &IntegerNode) -> Result<DerivationTreeOf<Ast>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::integer(node.0)?,
                Value::Integer(node.0),
            )),
            E_INT,
            vec![],
        ))
    }
}

impl<Ast: AstRoot> Visitor<BooleanNode, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
{
    fn visit(&mut self, node: &BooleanNode) -> Result<DerivationTreeOf<Ast>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::boolean(node.0)?,
                Value::Boolean(node.0),
            )),
            E_BOOL,
            vec![],
        ))
    }
}

impl<Ast: AstRoot + AsOpNums> Visitor<OpNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
    Judgement<NamedEnv<Ast>>: Derivable,
{
    fn visit(&mut self, node: &OpNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
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
                Ast::op_term(node.clone())?,
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

impl<Ast: AstRoot> Visitor<IfNode<Ast>, Result<DerivationTreeOf<Ast>>> for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &IfNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        let cond_tree = node.cond.apply_visitor(self)?;
        let (expr_tree, rule) = match cond_tree.judgement.eval_result()? {
            Value::Boolean(true) => node.t_branch.apply_visitor(self).map(|r| (r, E_IF_T)),
            Value::Boolean(false) => node.f_branch.apply_visitor(self).map(|r| (r, E_IF_F)),
            _ => Err(Error::NonBooleanValue),
        }?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::if_term(node.clone())?,
                expr_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![cond_tree, expr_tree],
        ))
    }
}

impl<Ast: AstRoot> Visitor<LetInNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &LetInNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        let tree_1 = node.expr_1.apply_visitor(self)?;
        let value_1 = tree_1.judgement.eval_result()?;
        let mut sub_visitor = UntypedDeriveVisitor::new(
            self.env
                .clone()
                .append_named(node.ident.clone(), value_1.clone()),
        );
        let tree_2 = node.expr_2.apply_visitor(&mut sub_visitor)?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::let_in_term(node.clone())?,
                tree_2.judgement.eval_result()?.clone(),
            )),
            E_LET,
            vec![tree_1, tree_2],
        ))
    }
}

impl<Ast: AstRoot> Visitor<FunctionNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &FunctionNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::function_term(node.clone())?,
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

impl<Ast: AstRoot> Visitor<LetRecInNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &LetRecInNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        let mut new_visitor = UntypedDeriveVisitor::new(self.env.clone().append_named(
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
                Ast::let_rec_in_term(node.clone())?,
                expr_tree.judgement.eval_result()?.clone(),
            )),
            E_LET_REC,
            vec![expr_tree],
        ))
    }
}

impl<Ast: AstRoot + AsParam> Visitor<ApplicationNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &ApplicationNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
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
        let mut new_visitor = UntypedDeriveVisitor::new(new_env);
        let app_tree = body.apply_visitor(&mut new_visitor)?;
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::application_term(node.clone())?,
                app_tree.judgement.eval_result()?.clone(),
            )),
            rule,
            vec![f_tree, p_tree, app_tree],
        ))
    }
}

impl<Ast: AstRoot> Visitor<NilListNode, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
{
    fn visit(&mut self, _node: &NilListNode) -> Result<DerivationTreeOf<Ast>> {
        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::nil_list()?,
                Value::NilList,
            )),
            E_NIL,
            vec![],
        ))
    }
}

impl<Ast: AstRoot> Visitor<ListConcatNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &ListConcatNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        let left_tree = node.lhs.apply_visitor(self)?;
        let left_result = left_tree.judgement.eval_result()?;
        let right_tree = node.rhs.apply_visitor(self)?;
        let right_result = right_tree.judgement.eval_result()?;

        Ok(DerivationTree::new(
            Judgement::EvalTo(EvalToJudgement::new(
                self.env.clone(),
                Ast::list_concat(node.clone())?,
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

impl<Ast: AstRoot> Visitor<ListPatternMatchNode<Ast>, Result<DerivationTreeOf<Ast>>>
    for UntypedDeriveVisitor<Ast>
where
    Self: Visitor<Ast, Result<DerivationTreeOf<Ast>>>,
{
    fn visit(&mut self, node: &ListPatternMatchNode<Ast>) -> Result<DerivationTreeOf<Ast>> {
        let expr_tree = node.expr.apply_visitor(self)?;

        match expr_tree.judgement.eval_result()? {
            Value::NilList => {
                let body_tree = node.nil_branch.apply_visitor(self)?;

                Ok((body_tree, E_MATCH_NIL))
            }
            Value::ConcatList(ConcatList { box lhs, box rhs }) => {
                let mut new_visitor = UntypedDeriveVisitor::new(
                    self.env
                        .clone()
                        .append_named(node.head_id.clone(), lhs.clone())
                        .append_named(node.tail_id.clone(), rhs.clone()),
                );

                let body_tree = node.list_branch.apply_visitor(&mut new_visitor)?;

                Ok((body_tree, E_MATCH_CONS))
            }
            _ => Err(Error::UnsupportedPattern),
        }
        .and_then(|(body_tree, rule)| {
            Ok(DerivationTree::new(
                Judgement::EvalTo(EvalToJudgement::new(
                    self.env.clone(),
                    Ast::list_pattern_match(node.clone())?,
                    body_tree.judgement.eval_result()?.clone(),
                )),
                rule,
                vec![expr_tree, body_tree],
            ))
        })
    }
}
