use crate::derive::{Derivable, DerivationTree, Result};
use crate::error::Error;
use crate::print::TokenBuffer;
use crate::systems::common::env::NamedEnv;
use crate::systems::common::judgement::{
    Judgement, LtIsJudgement, MinusIsJudgement, PlusIsJudgement, TimesIsJudgement,
};
use crate::systems::common::print::PrintVisitor;
use crate::systems::common::syntax::{AstRoot, Op};
use crate::systems::common::value::Value;
use crate::systems::eval_ml_3::visitor::DeriveVisitor;
use crate::visitor::{Visitable, Visitor};
use std::fmt::Result as FmtResult;

impl<Ast: AstRoot> Derivable for Judgement<NamedEnv<Ast>>
where
    DeriveVisitor<NamedEnv<Ast>>: Visitor<Ast, Result<DerivationTree<Self>>>,
    PrintVisitor: Visitor<Ast, FmtResult> + Visitor<Value<NamedEnv<Ast>>, FmtResult>,
{
    fn derive(self) -> Result<DerivationTree<Self>>
    where
        Self: Sized,
    {
        if let Judgement::EvalTo(eval_to) = self {
            let mut visitor = DeriveVisitor::new(eval_to.env.clone());
            let tree = eval_to.term.apply_visitor(&mut visitor)?;

            let value = tree.judgement.eval_result()?;

            if value.eq(&eval_to.value) {
                Ok(tree)
            } else {
                let mut term_visitor = PrintVisitor::new(1);
                eval_to.term.apply_visitor(&mut term_visitor)?;
                let term_buffer: Result<TokenBuffer> = term_visitor.into();

                let mut value_visitor = PrintVisitor::new(0);
                value.apply_visitor(&mut value_visitor)?;
                let value_buffer: Result<TokenBuffer> = value_visitor.into();

                let mut result_visitor = PrintVisitor::new(0);
                eval_to.value.apply_visitor(&mut result_visitor)?;
                let result_buffer: Result<TokenBuffer> = result_visitor.into();

                Err(Error::AssertionError(
                    term_buffer?.format(2),
                    value_buffer?.format_inline(),
                    result_buffer?.format_inline(),
                ))
            }
        } else {
            let op = match self {
                Judgement::PlusIs(PlusIsJudgement(a, b, c)) if a + b == c => Ok(Op::Plus),
                Judgement::PlusIs(PlusIsJudgement(a, b, c)) => Err(Error::AssertionError(
                    format!("{} plus {}", a, b),
                    c.to_string(),
                    (a + b).to_string(),
                )),
                Judgement::MinusIs(MinusIsJudgement(a, b, c)) if a - b == c => Ok(Op::Minus),
                Judgement::MinusIs(MinusIsJudgement(a, b, c)) => Err(Error::AssertionError(
                    format!("{} minus {}", a, b),
                    c.to_string(),
                    (a - b).to_string(),
                )),
                Judgement::TimesIs(TimesIsJudgement(a, b, c)) if a * b == c => Ok(Op::Times),
                Judgement::TimesIs(TimesIsJudgement(a, b, c)) => Err(Error::AssertionError(
                    format!("{} plus {}", a, b),
                    c.to_string(),
                    (a + b).to_string(),
                )),
                Judgement::LtIs(LtIsJudgement(a, b, c)) if (a < b) == c => Ok(Op::Lt),
                Judgement::LtIs(LtIsJudgement(a, b, c)) => Err(Error::AssertionError(
                    format!("{} plus {}", a, b),
                    c.to_string(),
                    (a < b).to_string(),
                )),
                _ => unreachable!(),
            }?;

            Ok(DerivationTree::new(self, op.to_rule(), vec![]))
        }
    }
}
