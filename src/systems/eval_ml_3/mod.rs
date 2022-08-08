use pest::iterators::Pairs;
use pest::Position;

use crate::derive::{Derivable, DerivationTree, ParseNextAs, System};
use crate::pest::Parser;
use crate::systems::common::env::NamedEnv;
use crate::systems::common::judgement::Judgement;
use crate::systems::eval_ml_3::parser::ASTParser;
use crate::systems::eval_ml_3::syntax::EvalML3Node;

pub mod derive;
pub mod parser;
pub mod rules;
pub mod syntax;

pub struct EvalML3;

impl System for EvalML3 {
    type Judgement = Judgement<NamedEnv<EvalML3Node>>;

    fn derive(src: &str) -> crate::derive::Result<DerivationTree<Self::Judgement>> {
        let mut pairs: Pairs<parser::Rule> = ASTParser::parse(parser::Rule::entry, src)?;
        let judgement: Self::Judgement = pairs.parse_next(Position::new(src, 0).unwrap())?;

        judgement.derive()
    }
}
