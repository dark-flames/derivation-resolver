use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest::error::Error;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Position;

use crate::derive::{Parse, ParseAs, ParseNextAs};
use crate::systems::common::env::{Env, TypedEnv};
use crate::systems::common::judgement::TypeJudgement;
use crate::systems::common::syntax::{
    ApplicationNode, AsListSeg, AsOpNums, AsParam, AstRoot, FunctionNode, Ident, IfNode, LetInNode,
    LetRecInNode, ListConcatNode, ListPatternMatchNode, Op, OpNode,
};
use crate::systems::common::ty::{ParseType, PolyType};
use crate::utils::{error_pos, error_span};

#[derive(Parser)]
#[grammar = "systems/common/grammar.pest"]
pub struct ASTParser;

type Result<T> = StdResult<T, Error<Rule>>;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(op_concat, Right),
            Operator::new(op_lt, Left),
            Operator::new(op_plus, Left) | Operator::new(op_minus, Left),
            Operator::new(op_times, Left),
        ])
    };
}

impl<Ast: AstRoot + AsParam + AsOpNums + AsListSeg> Parse<Rule> for Ast {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        let pos = entry_pair.as_span().start_pos();
        match entry_pair.as_rule() {
            Rule::integer => entry_pair
                .parse()
                .and_then(|i| Ast::integer(i).map_err(|e| e.with_pos(pos))),
            Rule::boolean => entry_pair
                .parse()
                .and_then(|b| Ast::boolean(b).map_err(|e| e.with_pos(pos))),
            Rule::ident => entry_pair
                .parse()
                .and_then(|i| Ast::variable(i).map_err(|e| e.with_pos(pos))),
            Rule::nil_list_term => Ast::nil_list().map_err(|e| e.with_pos(pos)),
            Rule::atom_term => entry_pair.into_inner().parse_next(pos),
            Rule::ap_term => {
                let mut inner = entry_pair.into_inner();
                let init = inner.parse_next(pos.clone());

                inner.fold(init, |lhs, current| {
                    lhs.and_then(|l| {
                        Ast::application_term(ApplicationNode {
                            f: Box::new(l),
                            p: Box::new(current.parse()?),
                        })
                        .map_err(|e| e.with_pos(pos.clone()))
                    })
                })
            }
            Rule::op_term => PREC_CLIMBER.climb(
                entry_pair.into_inner(),
                |pair| pair.parse(),
                |lhs, op, rhs| {
                    if op.as_rule() == Rule::op_concat {
                        Ast::list_concat(ListConcatNode {
                            lhs: Box::new(lhs?),
                            rhs: Box::new(rhs?),
                        })
                        .map_err(|e| e.with_pos(pos.clone()))
                    } else {
                        Ast::op_term(OpNode {
                            lhs: Box::new(lhs?),
                            op: match op.as_rule() {
                                Rule::op_plus => Op::Plus,
                                Rule::op_minus => Op::Minus,
                                Rule::op_times => Op::Times,
                                Rule::op_lt => Op::Lt,
                                _ => unreachable!(),
                            },
                            rhs: Box::new(rhs?),
                        })
                        .map_err(|e| e.with_pos(pos.clone()))
                    }
                },
            ),
            Rule::if_term => {
                let mut inner = entry_pair.into_inner();

                let (cond, pos) = inner.parse_next_with_pos(pos.clone())?;
                let (t_branch, pos) = inner.parse_next_with_pos(pos.clone())?;
                let f_branch = inner.parse_next(pos.clone())?;

                Ast::if_term(IfNode {
                    cond: Box::new(cond),
                    t_branch: Box::new(t_branch),
                    f_branch: Box::new(f_branch),
                })
                .map_err(|e| e.with_pos(pos.clone()))
            }
            Rule::let_in_term => {
                let mut inner = entry_pair.into_inner();

                let (ident, pos) = inner.parse_next_with_pos(pos.clone())?;
                let (expr_1, pos) = inner.parse_boxed_next_with_pos(pos.clone())?;
                let expr_2 = inner.parse_boxed_next(pos.clone())?;

                Ast::let_in_term(LetInNode {
                    ident,
                    expr_1,
                    expr_2,
                })
                .map_err(|e| e.with_pos(pos))
            }
            Rule::fun_term => {
                let mut inner = entry_pair.into_inner();

                let (bind, pos) = inner.parse_next_with_pos(pos.clone())?;
                let body = inner.parse_boxed_next(pos.clone())?;

                Ast::function_term(FunctionNode { bind, body }).map_err(|e| e.with_pos(pos))
            }
            Rule::let_rec_in_term => {
                let mut inner = entry_pair.into_inner();

                let (ident, pos) = inner.parse_next_with_pos(pos.clone())?;
                let (bind, pos) = inner.parse_next_with_pos(pos.clone())?;
                let (body, pos) = inner.parse_boxed_next_with_pos(pos.clone())?;
                let expr = inner.parse_boxed_next(pos.clone())?;

                Ast::let_rec_in_term(LetRecInNode {
                    ident,
                    bind,
                    body,
                    expr,
                })
                .map_err(|e| e.with_pos(pos))
            }
            Rule::list_match_term => {
                let mut inner = entry_pair.into_inner();

                let (expr, pos) = inner.parse_boxed_next_with_pos(pos.clone())?;
                let (nil_branch, pos) = inner.parse_boxed_next_with_pos(pos.clone())?;
                let (head_id, pos) = inner.parse_next_with_pos(pos.clone())?;
                inner.next();
                let (tail_id, pos) = inner.parse_next_with_pos(pos.clone())?;
                let list_branch = inner.parse_boxed_next(pos.clone())?;

                Ast::list_pattern_match(ListPatternMatchNode {
                    expr,
                    nil_branch,
                    head_id,
                    tail_id,
                    list_branch,
                })
                .map_err(|e| e.with_pos(pos))
            }
            _ => unreachable!(),
        }
    }
}

impl<Ast: AstRoot + AsParam + AsOpNums + AsListSeg> Parse<Rule> for TypedEnv<Ast> {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        match entry_pair.as_rule() {
            Rule::typed_env => Ok(entry_pair
                .into_inner()
                .map(|seg_pair| {
                    let seg_span = seg_pair.as_span();
                    let mut inner_rules = seg_pair.into_inner();
                    let (ident, pos): (Ident, Position) = match inner_rules.next() {
                        Some(r) if r.as_rule() == Rule::ident => r.parse_with_pos(),
                        _ => Err(error_span(seg_span, "expect an ident here".to_string())),
                    }?;

                    let ty: PolyType = inner_rules.parse_next(pos)?;

                    Ok((ident, ty))
                })
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .rev()
                .fold(TypedEnv::new(), |env, (ident, ty)| {
                    env.append_named(ident, ty)
                })),
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for ParseType {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        let start_pos = entry_pair.as_span().start_pos();
        match entry_pair.as_rule() {
            Rule::atom_ty => {
                let pair = entry_pair.into_inner().next().unwrap();

                match pair.as_rule() {
                    Rule::keyword_bool => Ok(ParseType::Bool),
                    Rule::keyword_int => Ok(ParseType::Integer),
                    Rule::ty_var => Ok(ParseType::Var(pair.as_str().to_string())),
                    Rule::mono_ty => pair.parse(),
                    _ => unreachable!(),
                }
            }
            Rule::list_ty => {
                let mut inner = entry_pair.into_inner();
                let nested: ParseType = inner.parse_next(start_pos.clone())?;

                match inner.next() {
                    Some(p) if p.as_rule() == Rule::keyword_list => {
                        Ok(ParseType::List(Box::new(nested)))
                    }
                    Some(_) => Err(error_pos(start_pos, "expect a list".to_string())),
                    None => Ok(nested),
                }
            }
            Rule::mono_ty => {
                let mut inner = entry_pair.into_inner().rev();
                let mut p_ty = inner.parse_next(start_pos.clone())?;
                while let Some(pair) = inner.next() {
                    match pair {
                        p if p.as_rule() == Rule::list_ty => {
                            p_ty = ParseType::Lambda(p.parse_boxed()?, Box::new(p_ty));
                        }
                        _ => return Err(error_pos(start_pos, "expect a type".to_string())),
                    }
                }

                Ok(p_ty)
            }
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for PolyType {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        match entry_pair.as_rule() {
            Rule::poly_ty => {
                let mut inner = entry_pair.into_inner();
                let mut binds = HashMap::new();
                loop {
                    match inner.next() {
                        Some(pair) if pair.as_rule() == Rule::mono_ty => {
                            let ty: ParseType = pair.parse()?;
                            break Ok(PolyType {
                                ty: ty.into_poly_ty().ty,
                                binds: binds.values().cloned().collect(),
                            });
                        }
                        Some(pair) if pair.as_rule() == Rule::ty_var => {
                            let new_var = binds.len();
                            let entry = binds.entry(pair.as_str().to_string());
                            if let Entry::Occupied(_) = entry {
                                break Err(error_span(pair.as_span(), "existed var".to_string()));
                            } else {
                                entry.or_insert(new_var);
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<Ast: AstRoot + AsParam + AsOpNums + AsListSeg> Parse<Rule> for TypeJudgement<TypedEnv<Ast>> {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        let span = entry_pair.as_span();
        let pos = span.start_pos();
        let entry_rule = entry_pair.as_rule();
        match entry_rule {
            Rule::typing_judgement => {
                let mut inner = entry_pair.into_inner();
                let (env, pos): (TypedEnv<Ast>, _) = inner.parse_next_with_pos(pos)?;
                let (term, pos) = inner.parse_next_with_pos(pos)?;
                let ty: ParseType = inner.parse_next(pos)?;
                let poly_ty = ty.into_poly_ty();
                Ok(TypeJudgement {
                    env,
                    term,
                    ty: poly_ty.ty,
                })
            }
            _ => unreachable!(),
        }
    }
}
