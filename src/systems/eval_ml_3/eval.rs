use crate::error::Error;
use crate::interface;
use crate::interface::DerivationTree;
use crate::systems::eval_ml_3::rules::*;
use crate::systems::eval_ml_3::syntax::*;

pub fn eval(env: &Env, node: &AstNode) -> interface::Result<(DerivationTree<Judgement>, Value)> {
    let (reason, premises, value) = match node {
        AstNode::Integer(i) => Ok((E_INT, vec![], Value::Integer(*i))),
        AstNode::Boolean(b) => Ok((E_BOOL, vec![], (Value::Boolean(*b)))),
        AstNode::Variable(v) => match env {
            Env::Terminal => Err(Error::UnknownIdentifier),
            Env::Segment(_, id, value) if id == v => Ok((E_VAR1, vec![], value.clone())),
            Env::Segment(next, ..) => {
                eval(next.as_ref(), node).map(|(tree, v)| (E_VAR2, vec![tree], v))
            }
        },
        AstNode::Op { lhs, op, rhs } => {
            let (lhs_tree, lhs_value) = eval(env, lhs.as_ref())?;
            let (rhs_tree, rhs_value) = eval(env, rhs.as_ref())?;
            let result = op.apply(&lhs_value, &rhs_value)?;

            Ok((
                match op {
                    Op::Plus => E_PLUS,
                    Op::Minus => E_BOOL,
                    Op::Times => E_TIMES,
                    Op::Lt => E_LT,
                },
                vec![
                    lhs_tree,
                    rhs_tree,
                    DerivationTree::new(
                        Judgement::from_op(*op, &lhs_value, &rhs_value, &result)?,
                        match op {
                            Op::Plus => B_PLUS,
                            Op::Minus => B_MINUS,
                            Op::Times => B_TIMES,
                            Op::Lt => B_LT,
                        },
                        vec![],
                    ),
                ],
                result,
            ))
        }
        AstNode::IfTerm {
            cond,
            t_branch,
            f_branch,
        } => {
            let (cond_tree, cond_value) = eval(env, cond.as_ref())?;
            let ((expr_tree, result), rule) = match cond_value {
                Value::Boolean(true) => eval(env, t_branch.as_ref()).map(|r| (r, E_IF_T)),
                Value::Boolean(false) => eval(env, f_branch.as_ref()).map(|r| (r, E_IF_F)),
                _ => Err(Error::NonBooleanValue),
            }?;

            Ok((rule, vec![cond_tree, expr_tree], result))
        }
        AstNode::LetInTerm {
            ident,
            expr_1,
            expr_2,
        } => {
            let (tree_1, value_1) = eval(env, expr_1.as_ref())?;
            let new_env = Env::Segment(Box::new(env.clone()), ident.clone(), value_1);
            let (tree_2, result) = eval(&new_env, expr_2.as_ref())?;

            Ok((E_LET, vec![tree_1, tree_2], result))
        }
        AstNode::Function { bind, body } => Ok((
            E_FUN,
            vec![],
            Value::Fun(Box::new(Function {
                env: env.clone(),
                bind: bind.clone(),
                body: body.clone(),
            })),
        )),
        AstNode::LetRecIn {
            ident,
            bind,
            body,
            expr,
        } => {
            let new_env = Env::Segment(
                Box::new(env.clone()),
                ident.clone(),
                Value::RecFun(Box::new(RecursiveFunction {
                    env: env.clone(),
                    ident: ident.clone(),
                    bind: bind.clone(),
                    body: body.clone(),
                })),
            );
            let (tree, result) = eval(&new_env, expr.as_ref())?;

            Ok((E_LET_REC, vec![tree], result))
        }
        AstNode::Application { f, p } => {
            let (f_tree, f_value) = eval(env, f.as_ref())?;
            let (p_tree, p_value) = eval(env, p.as_ref())?;

            let (bind, f_env, body, rule) = match &f_value {
                Value::Fun(box Function {
                    env: f_env,
                    bind,
                    body,
                }) => Ok((bind.clone(), f_env.clone(), body, E_APP)),
                Value::RecFun(box RecursiveFunction {
                    env: f_env,
                    ident,
                    bind,
                    body,
                }) => Ok((
                    bind.clone(),
                    Env::Segment(Box::new(env.clone()), ident.clone(), f_value.clone()),
                    body,
                    E_APP_REC,
                )),
                _ => Err(Error::ApplyOnNonFunctionValue),
            }?;

            let new_env = Env::Segment(Box::new(f_env), bind, p_value);

            let (app_tree, result) = eval(&new_env, body)?;

            Ok((rule, vec![f_tree, p_tree, app_tree], result))
        }
    }?;

    Ok((
        DerivationTree::new(
            Judgement::EvalTo {
                env: env.clone(),
                term: node.clone(),
                value: value.clone(),
            },
            reason,
            premises,
        ),
        value,
    ))
}
