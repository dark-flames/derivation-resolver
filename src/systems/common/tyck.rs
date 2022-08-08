use crate::derive::{DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::env::{Env, TypedEnv};
use crate::systems::common::judgement::TypeJudgement;
use crate::systems::common::rules::*;
use crate::systems::common::syntax::{
    ApplicationNode, AsOpNums, AsParam, AstRoot, BooleanNode, FunctionNode, IfNode, IntegerNode,
    LetInNode, LetRecInNode, ListConcatNode, ListPatternMatchNode, NilListNode, Op, OpNode,
    VariableNode,
};
use crate::systems::common::ty::{unify, MonoType, PolyType, Substitution};
use crate::visitor::{MutVisitable, Visitor};
use crate::Visitable;
use std::collections::HashSet;
use std::fmt::Debug;

pub type TyckTreeOf<Ast> = DerivationTree<TypeJudgement<TypedEnv<Ast>>>;
pub type TyckResult<Ast> = Result<(TyckTreeOf<Ast>, Substitution)>;

#[derive(Clone)]
pub struct TypeCheckVisitor<Ast: AstRoot> {
    pub env: TypedEnv<Ast>,
    pub ty_var_counter: usize,
    pub expected: Option<MonoType>,
}

impl<Ast: AstRoot> TypeCheckVisitor<Ast> {
    pub fn new(env: TypedEnv<Ast>, expected: Option<MonoType>) -> Self {
        Self {
            env,
            expected,
            ty_var_counter: 0,
        }
    }

    pub fn expect(&self, expected: Option<MonoType>) -> Self {
        Self {
            env: self.env.clone(),
            expected,
            ty_var_counter: self.ty_var_counter,
        }
    }

    pub fn substitute(&self, substitution: &mut Substitution, expected: Option<MonoType>) -> Self {
        let mut env = self.env.clone();
        env.apply_mut_visitor(substitution);
        TypeCheckVisitor {
            env,
            expected,
            ty_var_counter: self.ty_var_counter,
        }
    }

    pub fn new_ty_var(&mut self) -> PolyType {
        let index = self.ty_var_counter;
        self.ty_var_counter += 1;
        PolyType {
            binds: HashSet::from([index]),
            ty: MonoType::Var(index),
        }
    }

    pub fn apply_visitor<T: Visitable>(&mut self, target: &T, visitor: &mut Self) -> TyckResult<Ast>
    where
        Self: Visitor<T, TyckResult<Ast>>,
    {
        let r = target.apply_visitor(visitor);
        self.ty_var_counter = visitor.ty_var_counter;
        r
    }
}

pub fn tyck_as_error(node: &impl Debug, ty: &impl Debug) -> Error {
    Error::TypeCheckAsError(format!("{:?}", node), format!("{:?}", ty))
}

pub fn tyck_error(node: &impl Debug) -> Error {
    Error::TypeCheckError(format!("{:?}", node))
}

impl<Ast: AstRoot> Visitor<IntegerNode, TyckResult<Ast>> for TypeCheckVisitor<Ast> {
    fn visit(&mut self, node: &IntegerNode) -> TyckResult<Ast> {
        let ty = MonoType::Integer;
        let substitution = if let Some(expect) = self.expected.as_ref() {
            unify(expect, &ty)?
        } else {
            Substitution::default()
        };

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::integer(node.0)?,
                    ty: self.env.generalize(ty),
                },
                T_INT,
                vec![],
            ),
            substitution,
        ))
    }
}

impl<Ast: AstRoot> Visitor<BooleanNode, TyckResult<Ast>> for TypeCheckVisitor<Ast> {
    fn visit(&mut self, node: &BooleanNode) -> TyckResult<Ast> {
        let ty = MonoType::Bool;
        let substitution = if let Some(expect) = self.expected.as_ref() {
            unify(expect, &ty)?
        } else {
            Substitution::default()
        };

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::boolean(node.0)?,
                    ty: self.env.generalize(ty),
                },
                T_BOOL,
                vec![],
            ),
            substitution,
        ))
    }
}

impl<Ast: AstRoot> Visitor<VariableNode, TyckResult<Ast>> for TypeCheckVisitor<Ast> {
    fn visit(&mut self, node: &VariableNode) -> TyckResult<Ast> {
        let poly = self
            .env
            .look_up(&node.0)
            .ok_or_else(|| Error::UnknownIdentifier(node.0.clone()))?;

        let (ty, unifier) = if let Some(expect) = self.expected.as_ref() {
            if expect.instance_of(poly) {
                Ok((
                    self.env.generalize(expect.clone()),
                    unify(expect, &poly.ty)?,
                ))
            } else {
                Err(tyck_as_error(node, expect))
            }
        } else {
            Ok((poly.clone(), Substitution::default()))
        }?;

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::variable(node.0.clone())?,
                    ty,
                },
                T_VAR,
                vec![],
            ),
            unifier,
        ))
    }
}

impl<Ast: AstRoot + AsOpNums> Visitor<OpNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &OpNode<Ast>) -> TyckResult<Ast> {
        let node_ty = match node.op {
            Op::Plus | Op::Minus | Op::Times => MonoType::Integer,
            Op::Lt => MonoType::Bool,
        };

        let mut visitor = self.expect(Some(MonoType::Integer));
        let (lhs_tree, mut lhs_unifier) = self.apply_visitor(&node.lhs, &mut visitor)?;
        let (rhs_tree, mut rhs_unifier) = self.apply_visitor(&node.rhs, &mut visitor)?;
        lhs_unifier.apply_mut_visitor(&mut rhs_unifier)?;
        if let Some(expected) = self.expected.as_ref() {
            let mut unifier = unify(expected, &node_ty)?;
            lhs_unifier.apply_mut_visitor(&mut unifier)?;
        }

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::op_term(node.clone())?,
                    ty: self.env.generalize(node_ty),
                },
                match node.op {
                    Op::Plus => T_PLUS,
                    Op::Minus => T_MINUS,
                    Op::Times => T_TIMES,
                    Op::Lt => T_LT,
                },
                vec![lhs_tree, rhs_tree],
            ),
            lhs_unifier,
        ))
    }
}

impl<Ast: AstRoot + AsOpNums> Visitor<IfNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &IfNode<Ast>) -> TyckResult<Ast> {
        let mut cond_visitor = self.expect(Some(MonoType::Bool));
        let (mut cond_tree, mut cond_unifier) =
            self.apply_visitor(&node.cond, &mut cond_visitor)?;

        let mut branch_visitor = self.substitute(&mut cond_unifier, self.expected.clone());
        let (mut t_tree, mut t_unifier) = node.t_branch.apply_visitor(&mut branch_visitor)?;
        let (f_tree, mut f_unifier) = node.f_branch.apply_visitor(&mut branch_visitor)?;
        self.ty_var_counter = branch_visitor.ty_var_counter;
        let mut unifier = unify(&t_tree.judgement.ty.ty, &f_tree.judgement.ty.ty)?;

        unifier.apply_mut_visitor(&mut cond_unifier)?;
        unifier.apply_mut_visitor(&mut t_unifier)?;
        unifier.apply_mut_visitor(&mut f_unifier)?;
        cond_tree.apply_mut_visitor(&mut unifier);
        t_tree.apply_mut_visitor(&mut unifier);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::if_term(node.clone())?,
                    ty: t_tree.judgement.ty.clone(),
                },
                T_IF,
                vec![cond_tree, t_tree, f_tree],
            ),
            unifier,
        ))
    }
}

impl<Ast: AstRoot> Visitor<LetInNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &LetInNode<Ast>) -> TyckResult<Ast> {
        let mut visitor_1 = self.expect(None);
        let (tree_1, mut unifier_1) = self.apply_visitor(&node.expr_1, &mut visitor_1)?;
        let mut visitor_2 = self.substitute(&mut unifier_1, self.expected.clone());
        visitor_2.env = visitor_2
            .env
            .append_named(node.ident.clone(), tree_1.judgement.ty.clone());

        let (tree_2, mut unifier_2) = self.apply_visitor(&node.expr_2, &mut visitor_2)?;

        let mut ty = tree_2.judgement.ty.clone();

        unifier_1.apply_mut_visitor(&mut unifier_2)?;
        ty.apply_mut_visitor(&mut unifier_1);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::let_in_term(node.clone())?,
                    ty,
                },
                T_LET,
                vec![tree_1, tree_2],
            ),
            unifier_1,
        ))
    }
}

impl<Ast: AstRoot> Visitor<FunctionNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &FunctionNode<Ast>) -> TyckResult<Ast> {
        let (mut p_ty, expected_r_ty) = if let Some(expected_ty) = self.expected.as_ref() {
            if let MonoType::Lambda(box p_ty, box r_ty) = expected_ty {
                Ok((self.env.generalize(p_ty.clone()), Some(r_ty.clone())))
            } else {
                Err(tyck_as_error(node, expected_ty))
            }
        } else {
            Ok((self.new_ty_var(), None))
        }?;

        let mut visitor = self.expect(expected_r_ty);
        visitor.env = visitor.env.append_named(node.bind.clone(), p_ty.clone());

        let (b_tree, mut unifier) = self.apply_visitor(&node.body, &mut visitor)?;
        p_ty.apply_mut_visitor(&mut unifier);
        let mut ty = b_tree.judgement.ty.clone();
        ty.ty = MonoType::Lambda(Box::new(p_ty.ty.clone()), Box::new(ty.ty));
        ty.binds.extend(p_ty.binds.into_iter());

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::function_term(node.clone())?,
                    ty,
                },
                T_ABS,
                vec![b_tree],
            ),
            unifier,
        ))
    }
}

impl<Ast: AstRoot + AsParam> Visitor<ApplicationNode<Ast>, TyckResult<Ast>>
    for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &ApplicationNode<Ast>) -> TyckResult<Ast> {
        let mut result_ty = self.new_ty_var().ty;
        let mut f_visitor = self.expect(None);
        let (mut f_tree, mut f_unifier) = self.apply_visitor(&node.f, &mut f_visitor)?;

        let mut p_visitor = self.substitute(&mut f_unifier, None);
        let (mut p_tree, mut p_unifier) = self.apply_visitor(&node.p, &mut p_visitor)?;

        let mut original_f_ty = f_tree.judgement.ty.ty.clone();
        original_f_ty.apply_mut_visitor(&mut p_unifier);
        let expected_f_ty = MonoType::Lambda(
            Box::new(p_tree.judgement.ty.ty.clone()),
            Box::new(result_ty.clone()),
        );

        let mut unifier = unify(&original_f_ty, &expected_f_ty)?;
        result_ty.apply_mut_visitor(&mut unifier);
        let mut unifier = if let Some(expected_ty) = self.expected.as_ref() {
            let mut unifier = unify(&result_ty, expected_ty)?;
            result_ty.apply_mut_visitor(&mut unifier);

            unifier
        } else {
            unifier
        };
        f_tree.apply_mut_visitor(&mut unifier);
        p_tree.apply_mut_visitor(&mut unifier);
        let mut result_env = self.env.clone();
        result_env.apply_mut_visitor(&mut unifier);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::application_term(node.clone())?,
                    ty: result_env.generalize(result_ty),
                },
                T_APP,
                vec![f_tree, p_tree],
            ),
            unifier,
        ))
    }
}

impl<Ast: AstRoot> Visitor<LetRecInNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &LetRecInNode<Ast>) -> TyckResult<Ast> {
        let mut p_ty = self.new_ty_var();
        let mut r_ty = self.new_ty_var();

        let mut body_visitor = self.expect(Some(r_ty.ty.clone()));
        body_visitor.env = body_visitor
            .env
            .append_named(
                node.ident.clone(),
                self.env.generalize(MonoType::Lambda(
                    Box::new(p_ty.ty.clone()),
                    Box::new(r_ty.ty.clone()),
                )),
            )
            .append_named(node.bind.clone(), p_ty.clone().into());

        let (f_tree, mut f_unifier) = self.apply_visitor(&node.body, &mut body_visitor)?;
        p_ty.apply_mut_visitor(&mut f_unifier);
        r_ty.apply_mut_visitor(&mut f_unifier);

        let mut expr_visitor = self.substitute(&mut f_unifier, self.expected.clone());

        let bind_ty = expr_visitor.env.generalize(MonoType::Lambda(
            Box::new(p_ty.ty.clone()),
            Box::new(r_ty.ty.clone()),
        ));
        expr_visitor.env = expr_visitor.env.append_named(node.ident.clone(), bind_ty);

        let (expr_tree, mut expr_unifier) = self.apply_visitor(&node.expr, &mut expr_visitor)?;

        f_unifier.apply_mut_visitor(&mut expr_unifier)?;
        let mut ty = expr_tree.judgement.ty.clone();
        ty.apply_mut_visitor(&mut f_unifier);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::let_rec_in_term(node.clone())?,
                    ty,
                },
                T_LET_REC,
                vec![f_tree, expr_tree],
            ),
            f_unifier,
        ))
    }
}

impl<Ast: AstRoot> Visitor<NilListNode, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &NilListNode) -> TyckResult<Ast> {
        if let Some(MonoType::List(expected_ty)) = self.expected.as_ref() {
            Ok((
                DerivationTree::new(
                    TypeJudgement {
                        env: self.env.clone(),
                        term: Ast::nil_list()?,
                        ty: self.env.generalize(MonoType::List(expected_ty.clone())),
                    },
                    T_NIL,
                    vec![],
                ),
                Substitution::default(),
            ))
        } else {
            Err(tyck_error(node))
        }
    }
}

impl<Ast: AstRoot> Visitor<ListConcatNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &ListConcatNode<Ast>) -> TyckResult<Ast> {
        let expected_nest = self
            .expected
            .as_ref()
            .cloned()
            .map(|e| {
                if let MonoType::List(box nest) = e {
                    Ok(Some(nest))
                } else {
                    Err(tyck_as_error(node, &e))
                }
            })
            .unwrap_or(Ok(None))?;

        let mut item_visitor = self.expect(expected_nest);
        let (mut item_tree, mut item_unifier) = self.apply_visitor(&node.lhs, &mut item_visitor)?;

        let (list_tree, mut list_unifier) = node.rhs.apply_visitor(self)?;
        item_unifier.apply_mut_visitor(&mut list_unifier)?;
        item_tree.apply_mut_visitor(&mut list_unifier);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::list_concat(node.clone())?,
                    ty: list_tree.judgement.ty.clone(),
                },
                T_CONS,
                vec![item_tree, list_tree],
            ),
            item_unifier,
        ))
    }
}

impl<Ast: AstRoot> Visitor<ListPatternMatchNode<Ast>, TyckResult<Ast>> for TypeCheckVisitor<Ast>
where
    Self: Visitor<Ast, TyckResult<Ast>>,
{
    fn visit(&mut self, node: &ListPatternMatchNode<Ast>) -> TyckResult<Ast> {
        let mut target_visitor = self.expect(None);
        let (mut target_tree, mut unifier) = self.apply_visitor(&node.expr, &mut target_visitor)?;

        let mut nil_branch_visitor = self.substitute(&mut unifier, self.expected.clone());
        let (mut nil_tree, mut uil_unifier) =
            self.apply_visitor(&node.nil_branch, &mut nil_branch_visitor)?;
        target_tree.apply_mut_visitor(&mut uil_unifier);
        unifier.apply_mut_visitor(&mut uil_unifier)?;

        let nested_ty = if let MonoType::List(box ty) = &target_tree.judgement.ty.ty {
            Ok(PolyType {
                binds: target_tree.judgement.ty.binds.clone(),
                ty: ty.clone(),
            })
        } else {
            let ty = "List".to_string();
            Err(tyck_as_error(&node.expr, &ty))
        }?;

        let mut list_branch_visitor =
            self.substitute(&mut unifier, Some(nil_tree.judgement.ty.ty.clone()));
        list_branch_visitor.env = list_branch_visitor
            .env
            .append_named(node.head_id.clone(), nested_ty)
            .append_named(node.tail_id.clone(), target_tree.judgement.ty.clone());

        let (list_tree, mut list_unifier) =
            self.apply_visitor(&node.list_branch, &mut list_branch_visitor)?;
        unifier.apply_mut_visitor(&mut list_unifier)?;
        target_tree.apply_mut_visitor(&mut unifier);
        nil_tree.apply_mut_visitor(&mut unifier);

        Ok((
            DerivationTree::new(
                TypeJudgement {
                    env: self.env.clone(),
                    term: Ast::list_pattern_match(node.clone())?,
                    ty: target_tree.judgement.ty.clone(),
                },
                T_CONS,
                vec![target_tree, nil_tree, list_tree],
            ),
            unifier,
        ))
    }
}
