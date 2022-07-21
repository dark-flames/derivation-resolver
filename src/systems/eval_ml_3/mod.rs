use crate::interface::{Derivable, DerivationTree, ParseNextAs, System};
use crate::pest::Parser;
use crate::systems::eval_ml_3::parser::ASTParser;
use crate::systems::eval_ml_3::syntax::Judgement;
use pest::iterators::Pairs;
use pest::Span;

pub mod derive;
pub mod parser;
pub mod rules;
pub mod syntax;

pub struct EvalML3;

impl System for EvalML3 {
    type Judgement = Judgement;

    fn derive(src: &str) -> crate::interface::Result<DerivationTree<Self::Judgement>> {
        let mut pairs: Pairs<parser::Rule> = ASTParser::parse(parser::Rule::entry, src)?;
        let judgement: Judgement = pairs.parse_next_as(Span::new(src, 0, src.len()).unwrap())?;

        judgement.derive()
    }
}
