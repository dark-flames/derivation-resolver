use crate::interface::{Parse, ParseAs, ParseNextAs};
use crate::systems::eval_ml_3::syntax::{
    AstNode, Env, EvalToJudgement, Function, Ident, Judgement, Op, RecursiveFunction, Value,
};
use crate::utils::error_span;
use lazy_static::lazy_static;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::result::Result as StdResult;

#[derive(Parser)]
#[grammar = "systems/eval_ml_3/grammar.pest"]
pub struct ASTParser;

type Result<T> = StdResult<T, Error<Rule>>;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(op_lt, Left),
            Operator::new(op_plus, Left) | Operator::new(op_minus, Left),
            Operator::new(op_times, Left),
        ])
    };
}

impl Parse<Rule> for Env {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        let span = entry_pair.as_span();
        Ok(entry_pair
            .into_inner()
            .map(|seg_pair| {
                let seg_span = seg_pair.as_span();
                let mut inner_rules = seg_pair.into_inner();
                let ident: Ident = match inner_rules.next() {
                    Some(r) if r.as_rule() == Rule::ident => r.parse_as(),
                    _ => Err(error_span(seg_span, "expect an ident here".to_string())),
                }?;

                let value: Value = inner_rules.parse_next_as(span.clone())?;

                Ok((ident, value))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .rev()
            .fold(Env::Terminal, |env, (ident, value)| {
                Env::Segment(Box::new(env), ident, value)
            }))
    }
}

impl Parse<Rule> for Value {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        let pair = entry_pair.into_inner().next().unwrap();
        let span = pair.as_span();

        match pair.as_rule() {
            Rule::integer => pair.parse_as().map(Value::Integer),
            Rule::boolean => pair.parse_as().map(Value::Integer),
            Rule::value_fun => {
                let mut inner = pair.into_inner();

                Ok(Value::Fun(Box::new(Function {
                    env: inner.parse_next_as(span.clone())?,
                    bind: inner.parse_next_as(span.clone())?,
                    body: Box::new(inner.parse_next_as(span.clone())?),
                })))
            }
            Rule::value_rec_fun => {
                let mut inner = pair.into_inner();

                Ok(Value::RecFun(Box::new(RecursiveFunction {
                    env: inner.parse_next_as(span.clone())?,
                    ident: inner.parse_next_as(span.clone())?,
                    bind: inner.parse_next_as(span.clone())?,
                    body: Box::new(inner.parse_next_as(span.clone())?),
                })))
            }
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for AstNode {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        match entry_pair.as_rule() {
            Rule::integer => entry_pair.parse_as().map(AstNode::Integer),
            Rule::boolean => entry_pair.parse_as().map(AstNode::Boolean),
            Rule::ident => entry_pair.parse_as().map(AstNode::Variable),
            Rule::atom_term => {
                let span = entry_pair.as_span();
                entry_pair.into_inner().parse_next_as(span)
            }
            Rule::ap_term => {
                let span = entry_pair.as_span();
                let mut inner = entry_pair.into_inner();
                let init = inner.parse_next_as(span.clone());

                inner.fold(init, |lhs, current| {
                    lhs.and_then(move |l| {
                        Ok(AstNode::Application {
                            f: Box::new(l),
                            p: Box::new(current.parse_as()?),
                        })
                    })
                })
            }
            Rule::op_term => PREC_CLIMBER.climb(
                entry_pair.into_inner(),
                |pair| pair.parse_as(),
                |lhs, op, rhs| {
                    Ok(AstNode::Op {
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
                },
            ),
            Rule::if_term => {
                let span = entry_pair.as_span();
                let mut inner = entry_pair.into_inner();
                Ok(AstNode::IfTerm {
                    cond: Box::new(inner.parse_next_as(span.clone())?),
                    t_branch: Box::new(inner.parse_next_as(span.clone())?),
                    f_branch: Box::new(inner.parse_next_as(span.clone())?),
                })
            }
            Rule::let_in_term => {
                let span = entry_pair.as_span();
                let mut inner = entry_pair.into_inner();
                Ok(AstNode::LetInTerm {
                    ident: inner.parse_next_as(span.clone())?,
                    expr_1: Box::new(inner.parse_next_as(span.clone())?),
                    expr_2: Box::new(inner.parse_next_as(span.clone())?),
                })
            }
            Rule::fun_term => {
                let span = entry_pair.as_span();
                let mut inner = entry_pair.into_inner();
                Ok(AstNode::Function {
                    bind: inner.parse_next_as(span.clone())?,
                    body: Box::new(inner.parse_next_as(span.clone())?),
                })
            }
            Rule::let_rec_in_term => {
                let span = entry_pair.as_span();
                let mut inner = entry_pair.into_inner();
                Ok(AstNode::LetRecIn {
                    ident: inner.parse_next_as(span.clone())?,
                    bind: inner.parse_next_as(span.clone())?,
                    body: Box::new(inner.parse_next_as(span.clone())?),
                    expr: Box::new(inner.parse_next_as(span.clone())?),
                })
            }
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for Judgement {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        let entry_span = entry_pair.as_span();
        let entry_rule = entry_pair.as_rule();
        match entry_rule {
            Rule::eval_to_judgement => {
                let mut inner_rules: Pairs<Rule> = entry_pair.into_inner();

                Ok(Judgement::EvalTo(EvalToJudgement::new(
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span)?,
                )))
            }
            Rule::plus_is_judgement | Rule::minus_is_judgement | Rule::times_is_judgement => {
                let mut inner_rules: Pairs<Rule> = entry_pair.into_inner();
                let (a, b, c) = (
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span.clone())?,
                );

                if inner_rules.next().is_none() {
                    Ok(match entry_rule {
                        Rule::plus_is_judgement => Judgement::PlusIs(a, b, c),
                        Rule::minus_is_judgement => Judgement::MinusIs(a, b, c),
                        Rule::times_is_judgement => Judgement::TimesIs(a, b, c),
                        _ => unreachable!(),
                    })
                } else {
                    Err(error_span(
                        entry_span,
                        "expect actual 3 parameters in this judgement".to_string(),
                    ))
                }
            }

            Rule::lt_is_judgement => {
                let mut inner_rules: Pairs<Rule> = entry_pair.into_inner();

                let (a, b, c) = (
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span.clone())?,
                    inner_rules.parse_next_as(entry_span.clone())?,
                );

                if inner_rules.next().is_none() {
                    Ok(Judgement::LtIs(a, b, c))
                } else {
                    Err(error_span(
                        entry_span,
                        "expect actual 3 parameters in this judgement".to_string(),
                    ))
                }
            }
            _ => unreachable!(),
        }
    }
}
