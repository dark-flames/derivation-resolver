use crate::derive::{DerivationTree, ParseNextAs};
use crate::derive::{Result, System};
use crate::pest::Parser;
use crate::systems::common::env::TypedEnv;
use crate::systems::common::judgement::TypeJudgement;
use crate::systems::common::parser::ASTParser;
use crate::systems::common::parser::Rule;
use crate::systems::common::ty::FreeTypeVarVisitor;
use crate::systems::common::tyck::TypeCheckVisitor;
use crate::systems::poly_ml_4::syntax::PolyML4Node;
use crate::visitor::{MutVisitable, Visitable};
use pest::iterators::Pairs;
use pest::Position;

mod syntax;
mod tyck;

pub struct PolyML4;

impl System for PolyML4 {
    type Judgement = TypeJudgement<TypedEnv<PolyML4Node>>;

    fn derive(src: &str) -> Result<DerivationTree<Self::Judgement>> {
        let mut pairs: Pairs<Rule> = ASTParser::parse(Rule::entry, src)?;
        let judgement: Self::Judgement = pairs.parse_next(Position::new(src, 0).unwrap())?;

        let mut free_var_visitor = FreeTypeVarVisitor::default();
        let mut visitor = TypeCheckVisitor::new(
            judgement.env.clone(),
            Some(judgement.ty.clone()),
            judgement.ty.apply_visitor(&mut free_var_visitor).len(),
        );

        judgement
            .term
            .apply_visitor(&mut visitor)
            .map(|(mut tree, mut unifier)| {
                tree.apply_mut_visitor(&mut unifier);
                tree
            })
    }
}
