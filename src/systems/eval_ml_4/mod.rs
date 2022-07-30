use crate::derive::{Derivable, DerivationTree, ParseNextAs, Result};
use crate::pest::Parser;
use crate::systems::common::env::NamedEnv;
use crate::systems::common::judgement::Judgement;
use crate::systems::eval_ml_4::parser::ASTParser;
use crate::systems::eval_ml_4::syntax::EvalML4Node;
use crate::System;
use pest::iterators::Pairs;
use pest::Position;

mod derive;
mod parser;
mod rules;
mod syntax;
mod visitor;

pub struct EvalML4;

impl System for EvalML4 {
    type Judgement = Judgement<NamedEnv<EvalML4Node>>;

    fn derive(src: &str) -> Result<DerivationTree<Self::Judgement>> {
        let mut pairs: Pairs<parser::Rule> = ASTParser::parse(parser::Rule::entry, src)?;
        let judgement: Self::Judgement = pairs.parse_next(Position::new(src, 0).unwrap())?;

        judgement.derive()
    }
}
