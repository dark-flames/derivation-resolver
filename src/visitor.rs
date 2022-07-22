use crate::ToToken;
use std::fmt::Debug;

pub trait AstNode: Clone + Eq + PartialEq + Debug + ToToken {
    fn apply<R, T: Visitor<Self, R>>(&self, visitor: &mut T) -> R
    where
        Self: Sized,
    {
        visitor.visit(self)
    }
}

pub trait Visitor<Node: AstNode, R = ()> {
    fn visit(&mut self, node: &Node) -> R;
}
