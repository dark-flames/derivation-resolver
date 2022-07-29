use crate::derive::{Parse, ParseAs, ParseNextAs};
use crate::systems::common::env::NamedEnv;
use crate::systems::common::judgement::{
    EvalToJudgement, Judgement, LtIsJudgement, MinusIsJudgement, PlusIsJudgement, TimesIsJudgement,
};
use crate::systems::common::syntax::{
    ApplicationNode, BooleanNode, FunctionNode, Ident, IfNode, IntegerNode, LetInNode,
    LetRecInNode, Op, OpNode, VariableNode,
};
use crate::systems::common::value::{Function, RecursiveFunction, Value};
use crate::systems::eval_ml_3::syntax::EvalML3Node;
use crate::utils::error_span;
use lazy_static::lazy_static;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Position;
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

impl Parse<Rule> for NamedEnv<EvalML3Node> {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(entry_pair
            .into_inner()
            .map(|seg_pair| {
                let seg_span = seg_pair.as_span();
                let mut inner_rules = seg_pair.into_inner();
                let (ident, pos): (Ident, Position) = match inner_rules.next() {
                    Some(r) if r.as_rule() == Rule::ident => r.parse_with_pos(),
                    _ => Err(error_span(seg_span, "expect an ident here".to_string())),
                }?;

                let value: Value<NamedEnv<EvalML3Node>> = inner_rules.parse_next(pos)?;

                Ok((ident, value))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .rev()
            .fold(NamedEnv::Terminal, |env, (ident, value)| {
                NamedEnv::Segment(Box::new(env), (ident, value))
            }))
    }
}

impl Parse<Rule> for Value<NamedEnv<EvalML3Node>> {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        let pair = entry_pair.into_inner().next().unwrap();
        let start_pos = pair.as_span().start_pos();

        match pair.as_rule() {
            Rule::integer => pair.parse().map(Value::Integer),
            Rule::boolean => pair.parse().map(Value::Integer),
            Rule::value_fun => {
                let mut inner = pair.into_inner();
                let (env, pos) = inner.parse_next_with_pos(start_pos)?;
                let (bind, pos) = inner.parse_next_with_pos(pos)?;
                let body = Box::new(inner.parse_next(pos)?);

                Ok(Value::Fun(Box::new(Function { env, bind, body })))
            }
            Rule::value_rec_fun => {
                let mut inner = pair.into_inner();

                let (env, pos) = inner.parse_next_with_pos(start_pos)?;
                let (ident, pos) = inner.parse_next_with_pos(pos)?;
                let (bind, pos) = inner.parse_next_with_pos(pos)?;
                let body = inner.parse_next(pos)?;

                Ok(Value::RecFun(Box::new(RecursiveFunction {
                    env,
                    ident,
                    bind,
                    body,
                })))
            }
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for EvalML3Node {
    fn parse(entry_pair: Pair<Rule>) -> Result<Self>
    where
        Self: Sized,
    {
        let pos = entry_pair.as_span().start_pos();
        match entry_pair.as_rule() {
            Rule::integer => entry_pair
                .parse()
                .map(|i| EvalML3Node::Integer(IntegerNode(i))),
            Rule::boolean => entry_pair
                .parse()
                .map(|b| EvalML3Node::Boolean(BooleanNode(b))),
            Rule::ident => entry_pair
                .parse()
                .map(|i| EvalML3Node::Variable(VariableNode(i))),
            Rule::atom_term => entry_pair.into_inner().parse_next(pos),
            Rule::ap_term => {
                let mut inner = entry_pair.into_inner();
                let init = inner.parse_next(pos);

                inner.fold(init, |lhs, current| {
                    lhs.and_then(move |l| {
                        Ok(EvalML3Node::ApplicationTerm(ApplicationNode {
                            f: Box::new(l),
                            p: Box::new(current.parse()?),
                        }))
                    })
                })
            }
            Rule::op_term => PREC_CLIMBER.climb(
                entry_pair.into_inner(),
                |pair| pair.parse(),
                |lhs, op, rhs| {
                    Ok(EvalML3Node::OpTerm(OpNode {
                        lhs: Box::new(lhs?),
                        op: match op.as_rule() {
                            Rule::op_plus => Op::Plus,
                            Rule::op_minus => Op::Minus,
                            Rule::op_times => Op::Times,
                            Rule::op_lt => Op::Lt,
                            _ => unreachable!(),
                        },
                        rhs: Box::new(rhs?),
                    }))
                },
            ),
            Rule::if_term => {
                let mut inner = entry_pair.into_inner();

                let (cond, pos) = inner.parse_next_with_pos(pos)?;
                let (t_branch, pos) = inner.parse_next_with_pos(pos)?;
                let f_branch = inner.parse_next(pos)?;

                Ok(EvalML3Node::IfTerm(IfNode {
                    cond,
                    t_branch,
                    f_branch,
                }))
            }
            Rule::let_in_term => {
                let mut inner = entry_pair.into_inner();

                let (ident, pos) = inner.parse_next_with_pos(pos)?;
                let (expr_1, pos) = inner.parse_next_with_pos(pos)?;
                let expr_2 = inner.parse_next(pos)?;

                Ok(EvalML3Node::LetInTerm(LetInNode {
                    ident,
                    expr_1,
                    expr_2,
                }))
            }
            Rule::fun_term => {
                let mut inner = entry_pair.into_inner();

                let (bind, pos) = inner.parse_next_with_pos(pos)?;
                let body = inner.parse_next(pos)?;

                Ok(EvalML3Node::FunctionTerm(FunctionNode { bind, body }))
            }
            Rule::let_rec_in_term => {
                let mut inner = entry_pair.into_inner();

                let (ident, pos) = inner.parse_next_with_pos(pos)?;
                let (bind, pos) = inner.parse_next_with_pos(pos)?;
                let (body, pos) = inner.parse_next_with_pos(pos)?;
                let expr = inner.parse_next(pos)?;

                Ok(EvalML3Node::LetRecInTerm(LetRecInNode {
                    ident,
                    bind,
                    body,
                    expr,
                }))
            }
            _ => unreachable!(),
        }
    }
}

impl Parse<Rule> for Judgement<NamedEnv<EvalML3Node>> {
    fn parse(entry_pair: Pair<Rule>) -> StdResult<Self, Error<Rule>>
    where
        Self: Sized,
    {
        let span = entry_pair.as_span();
        let pos = span.start_pos();
        let entry_rule = entry_pair.as_rule();
        match entry_rule {
            Rule::eval_to_judgement => {
                let mut inner: Pairs<Rule> = entry_pair.into_inner();
                let (env, pos) = inner.parse_next_with_pos(pos)?;
                let (term, pos) = inner.parse_next_with_pos(pos)?;
                let value = inner.parse_next(pos)?;
                Ok(Judgement::EvalTo(EvalToJudgement::new(env, term, value)))
            }
            Rule::plus_is_judgement | Rule::minus_is_judgement | Rule::times_is_judgement => {
                let mut inner: Pairs<Rule> = entry_pair.into_inner();
                let (a, pos) = inner.parse_next_with_pos(pos)?;
                let (b, pos) = inner.parse_next_with_pos(pos)?;
                let c = inner.parse_next(pos)?;

                if inner.next().is_none() {
                    Ok(match entry_rule {
                        Rule::plus_is_judgement => Judgement::PlusIs(PlusIsJudgement(a, b, c)),
                        Rule::minus_is_judgement => Judgement::MinusIs(MinusIsJudgement(a, b, c)),
                        Rule::times_is_judgement => Judgement::TimesIs(TimesIsJudgement(a, b, c)),
                        _ => unreachable!(),
                    })
                } else {
                    Err(error_span(
                        span,
                        "expect actual 3 parameters in this judgement".to_string(),
                    ))
                }
            }

            Rule::lt_is_judgement => {
                let mut inner: Pairs<Rule> = entry_pair.into_inner();

                let (a, pos) = inner.parse_next_with_pos(pos)?;
                let (b, pos) = inner.parse_next_with_pos(pos)?;
                let c = inner.parse_next(pos)?;

                if inner.next().is_none() {
                    Ok(Judgement::LtIs(LtIsJudgement(a, b, c)))
                } else {
                    Err(error_span(
                        span,
                        "expect actual 3 parameters in this judgement".to_string(),
                    ))
                }
            }
            _ => unreachable!(),
        }
    }
}
