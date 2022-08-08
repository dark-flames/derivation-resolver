use replace_with::replace_with;
use std::cmp::{max, min};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use crate::derive::{DerivationTree, Result};
use crate::error::Error;
use crate::systems::common::env::TypedEnv;
use crate::systems::common::judgement::TypeJudgement;
use crate::systems::common::syntax::AstRoot;
use crate::visitor::{MutVisitable, MutVisitor, Visitor};
use crate::Visitable;

pub fn unification_error(ty_1: &impl Debug, ty_2: &impl Debug) -> Error {
    Error::UnificationError(format!("{:?}", ty_1), format!("{:?}", ty_2))
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum MonoType {
    Integer,
    Bool,
    Var(usize),
    Lambda(Box<MonoType>, Box<MonoType>),
    List(Box<MonoType>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct PolyType {
    pub binds: HashSet<usize>,
    pub ty: MonoType,
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Substitution {
    map: HashMap<usize, MonoType>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct FreeTypeVarVisitor {}

pub type FreeVariables = HashSet<usize>;

impl Substitution {
    pub fn new(v: impl IntoIterator<Item = (usize, MonoType)>) -> Result<Self> {
        v.into_iter()
            .fold(Ok(HashMap::new()), |result, (id, ty)| {
                result.and_then(|mut map| {
                    let entry = map.entry(id);
                    if let Entry::Occupied(_) = entry {
                        Err(Error::SubstitutionError)
                    } else {
                        entry.or_insert(ty);
                        Ok(map)
                    }
                })
            })
            .map(|map| Substitution { map })
    }
}

impl MonoType {
    pub fn instance_of(&self, poly: &PolyType) -> bool {
        if let Ok(substitution) = unify(self, &poly.ty) {
            let ids: HashSet<usize> = substitution.map.keys().cloned().collect();
            ids.eq(&poly.binds)
        } else {
            false
        }
    }
}

impl Visitable for MonoType {}
impl Visitable for PolyType {}
impl Visitable for Substitution {}
impl MutVisitable for MonoType {}
impl MutVisitable for PolyType {}
impl MutVisitable for Substitution {}

impl From<MonoType> for PolyType {
    fn from(ty: MonoType) -> Self {
        PolyType {
            binds: Default::default(),
            ty,
        }
    }
}

impl MutVisitor<MonoType, ()> for Substitution {
    fn visit(&mut self, node: &mut MonoType) {
        match node {
            MonoType::Integer => {}
            MonoType::Bool => {}
            MonoType::Var(id) => {
                if let Some(ty) = self.map.get(id) {
                    let ty = ty.clone();
                    *node = ty;
                };
            }
            MonoType::Lambda(ty_1, ty_2) => {
                ty_1.apply_mut_visitor(self);
                ty_2.apply_mut_visitor(self);
            }
            MonoType::List(ty) => {
                ty.apply_mut_visitor(self);
            }
        };
    }
}
impl MutVisitor<PolyType, ()> for Substitution {
    fn visit(&mut self, node: &mut PolyType) {
        node.ty.apply_mut_visitor(self);
        let mut free_visitor = FreeTypeVarVisitor::default();
        let free_vars = node.ty.apply_visitor(&mut free_visitor);
        replace_with(&mut node.binds, HashSet::default, |binds| {
            binds
                .into_iter()
                .filter(|id| free_vars.contains(id))
                .collect()
        })
    }
}
impl<Ast: AstRoot> MutVisitor<TypedEnv<Ast>, ()> for Substitution {
    fn visit(&mut self, node: &mut TypedEnv<Ast>) {
        node.env.iter_mut().for_each(|(_, ty)| {
            ty.apply_mut_visitor(self);
        });
    }
}
impl<Ast: AstRoot> MutVisitor<TypeJudgement<TypedEnv<Ast>>, ()> for Substitution {
    fn visit(&mut self, node: &mut TypeJudgement<TypedEnv<Ast>>) {
        //node.env.apply_mut_visitor(self);
        node.ty.apply_mut_visitor(self);
    }
}
impl<Ast: AstRoot> MutVisitor<DerivationTree<TypeJudgement<TypedEnv<Ast>>>, ()> for Substitution {
    fn visit(&mut self, node: &mut DerivationTree<TypeJudgement<TypedEnv<Ast>>>) {
        node.judgement.apply_mut_visitor(self);
        node.premises
            .iter_mut()
            .for_each(|t| t.apply_mut_visitor(self))
    }
}
impl MutVisitor<Substitution, Result<()>> for Substitution {
    fn visit(&mut self, node: &mut Substitution) -> Result<()> {
        node.map.iter_mut().for_each(|(_, ty)| {
            ty.apply_mut_visitor(self);
        });

        for (i, ty) in self.map.iter() {
            let entry = node.map.entry(*i);

            if let Entry::Occupied(o) = entry {
                let mut sub = unify(o.get(), ty)?;
                node.apply_mut_visitor(&mut sub)?;
            } else {
                entry.or_insert_with(|| ty.clone());
            }
        }

        Ok(())
    }
}

impl Visitor<MonoType, FreeVariables> for FreeTypeVarVisitor {
    fn visit(&mut self, node: &MonoType) -> FreeVariables {
        match node {
            MonoType::Var(i) => HashSet::from([*i]),
            MonoType::Lambda(ty_1, ty_2) => {
                let mut free_vars = ty_1.apply_visitor(self);
                free_vars.extend(ty_2.apply_visitor(self));
                free_vars
            }
            MonoType::List(ty) => ty.apply_visitor(self),
            _ => HashSet::new(),
        }
    }
}
impl Visitor<PolyType, FreeVariables> for FreeTypeVarVisitor {
    fn visit(&mut self, node: &PolyType) -> FreeVariables {
        let free_var = node.ty.apply_visitor(self);
        // Free(p) = Free(m) - p.binds
        free_var.difference(&node.binds).cloned().collect()
    }
}
impl<Ast: AstRoot> Visitor<TypedEnv<Ast>, FreeVariables> for FreeTypeVarVisitor {
    fn visit(&mut self, node: &TypedEnv<Ast>) -> FreeVariables {
        node.env.iter().fold(HashSet::new(), |mut acc, (_, ty)| {
            acc.extend(ty.apply_visitor(self));
            acc
        })
    }
}

pub fn unify(ty_1: &MonoType, ty_2: &MonoType) -> Result<Substitution> {
    match (ty_1, ty_2) {
        (ty_1, ty_2) if ty_1.eq(ty_2) => Ok(Substitution::default()),
        (MonoType::Var(var_1), MonoType::Var(var_2)) => {
            Substitution::new([(max(*var_1, *var_2), MonoType::Var(min(*var_1, *var_2)))])
        }
        (ty_1, MonoType::Var(var_2)) => Substitution::new([(*var_2, ty_1.clone())]),
        (MonoType::Var(var_1), ty_2) => Substitution::new([(*var_1, ty_2.clone())]),
        (MonoType::List(listed_ty_1), MonoType::List(listed_ty_2)) => {
            unify(listed_ty_1, listed_ty_2)
        }
        (MonoType::Lambda(p_1, b_1), MonoType::Lambda(p_2, b_2)) => {
            let mut p_unifier = unify(p_1, p_2)?;
            let mut b_unifier = unify(b_1, b_2)?;

            p_unifier.apply_mut_visitor(&mut b_unifier)?;

            Ok(p_unifier)
        }
        _ => Err(unification_error(ty_1, ty_2)),
    }
}

#[test]
fn test_unify() {
    fn test_pair(mut ty_1: MonoType, mut ty_2: MonoType, expect: MonoType) {
        let mut unifier = unify(&ty_1, &ty_2).unwrap();
        unifier.visit(&mut ty_1);
        unifier.visit(&mut ty_2);
        assert_eq!(ty_1, expect);
        assert_eq!(ty_2, expect);
    }

    test_pair(MonoType::Var(0), MonoType::Integer, MonoType::Integer);
    test_pair(MonoType::Bool, MonoType::Var(0), MonoType::Bool);
    test_pair(
        MonoType::Lambda(Box::new(MonoType::Integer), Box::new(MonoType::Var(0))),
        MonoType::Lambda(Box::new(MonoType::Var(0)), Box::new(MonoType::Integer)),
        MonoType::Lambda(Box::new(MonoType::Integer), Box::new(MonoType::Integer)),
    );
    test_pair(
        MonoType::Lambda(Box::new(MonoType::Var(1)), Box::new(MonoType::Var(0))),
        MonoType::Lambda(Box::new(MonoType::Var(0)), Box::new(MonoType::Var(2))),
        MonoType::Lambda(Box::new(MonoType::Var(0)), Box::new(MonoType::Var(0))),
    );
}
