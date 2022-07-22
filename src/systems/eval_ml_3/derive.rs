use crate::derive::{Derivable, DerivationTree};
use crate::error::Error;
use crate::systems::common::env::NamedEnv;
use crate::systems::common::judgement::Judgement;
use crate::systems::common::syntax::Op;
use crate::systems::eval_ml_3::visitor::DeriveVisitor;
use crate::visitor::Visitor;
use crate::{derive, ToToken};

impl Derivable for Judgement<NamedEnv> {
    fn derive(self) -> derive::Result<DerivationTree<Self>>
    where
        Self: Sized,
    {
        if let Judgement::EvalTo(eval_to) = self {
            let mut visitor = DeriveVisitor::new(eval_to.env.clone());
            let tree = visitor.visit(&eval_to.term)?;

            let value = tree.judgement.eval_result()?;

            if value.eq(&eval_to.value) {
                Ok(tree)
            } else {
                Err(Error::AssertionError(
                    eval_to.term.token_buffer(1)?.format(2),
                    value.token_buffer(0)?.format_inline(),
                    eval_to.value.token_buffer(0)?.format_inline(),
                ))
            }
        } else {
            let op = match self {
                Judgement::PlusIs(a, b, c) if a + b == c => Ok(Op::Plus),
                Judgement::PlusIs(a, b, c) => Err(Error::AssertionError(
                    format!("{} plus {}", a, b),
                    c.to_string(),
                    (a + b).to_string(),
                )),
                Judgement::MinusIs(a, b, c) if a - b == c => Ok(Op::Minus),
                Judgement::MinusIs(a, b, c) => Err(Error::AssertionError(
                    format!("{} minus {}", a, b),
                    c.to_string(),
                    (a - b).to_string(),
                )),
                Judgement::TimesIs(a, b, c) if a * b == c => Ok(Op::Times),
                Judgement::TimesIs(a, b, c) => Err(Error::AssertionError(
                    format!("{} plus {}", a, b),
                    c.to_string(),
                    (a + b).to_string(),
                )),
                Judgement::LtIs(a, b, c) if (a < b) == c => Ok(Op::Lt),
                Judgement::LtIs(a, b, c) => Err(Error::AssertionError(
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
