use std::fmt::Debug;

pub trait Visitable: Clone + Eq + PartialEq + Debug {
    fn apply_visitor<R, T: Visitor<Self, R>>(&self, visitor: &mut T) -> R
    where
        Self: Sized,
    {
        visitor.visit(self)
    }
}

pub trait MutVisitable: Clone + Eq + PartialEq + Debug {
    fn apply_mut_visitor<R, T: MutVisitor<Self, R>>(&mut self, visitor: &mut T) -> R
    where
        Self: Sized,
    {
        visitor.visit(self)
    }
}

pub trait Visitor<Node: Visitable, R = ()> {
    fn visit(&mut self, node: &Node) -> R;
}

pub trait MutVisitor<Node: MutVisitable, R = ()> {
    fn visit(&mut self, node: &mut Node) -> R;
}
