use crate::systems::common::env::TypedEnv;
use crate::systems::common::syntax::*;
use crate::systems::common::tyck::{TyckResult, TypeCheckVisitor};
use crate::systems::poly_ml_4::syntax::PolyML4Node;
use crate::visitor::Visitor;
use crate::{PrintVisitor, Visitable};

impl Visitor<PolyML4Node, TyckResult<PolyML4Node>> for TypeCheckVisitor<PolyML4Node> {
    fn visit(&mut self, node: &PolyML4Node) -> TyckResult<PolyML4Node> {
        match node {
            PolyML4Node::Integer(n) => n.apply_visitor(self),
            PolyML4Node::Boolean(n) => n.apply_visitor(self),
            PolyML4Node::Variable(n) => n.apply_visitor(self),
            PolyML4Node::OpTerm(n) => n.apply_visitor(self),
            PolyML4Node::IfTerm(n) => n.apply_visitor(self),
            PolyML4Node::LetInTerm(n) => n.apply_visitor(self),
            PolyML4Node::FunctionTerm(n) => n.apply_visitor(self),
            PolyML4Node::ApplicationTerm(n) => n.apply_visitor(self),
            PolyML4Node::LetRecInTerm(n) => n.apply_visitor(self),
            PolyML4Node::NilListTerm(n) => n.apply_visitor(self),
            PolyML4Node::ListConcatTerm(n) => n.apply_visitor(self),
            PolyML4Node::ListPatternMatchTerm(n) => n.apply_visitor(self),
        }
    }
}

#[test]
fn test_tyck_1() {
    use crate::systems::common::ty::MonoType;
    let term = PolyML4Node::LetInTerm(LetInNode {
        ident: "id".to_string(),
        expr_1: Box::new(PolyML4Node::FunctionTerm(FunctionNode {
            bind: "x".to_string(),
            body: Box::new(PolyML4Node::Variable(VariableNode("x".to_string()))),
        })),
        expr_2: Box::new(PolyML4Node::ApplicationTerm(ApplicationNode {
            f: Box::new(PolyML4Node::Variable(VariableNode("id".to_string()))),
            p: Box::new(PolyML4Node::Variable(VariableNode("id".to_string()))),
        })),
    });

    let mut visitor = TypeCheckVisitor::new(
        TypedEnv::new(),
        Some(MonoType::Lambda(
            Box::new(MonoType::Bool),
            Box::new(MonoType::Bool),
        )),
    );

    let (tree, _) = term.apply_visitor(&mut visitor).unwrap();

    let mut print_visitor = PrintVisitor::new(0);
    tree.apply_visitor(&mut print_visitor).unwrap();
    let result = print_visitor.buffer.format(2);
    print!("{}", result);
}

#[test]
fn test_tyck_2() {
    use crate::systems::common::ty::MonoType;
    let term = PolyML4Node::LetInTerm(LetInNode {
        ident: "gt".to_string(),
        expr_1: Box::new(PolyML4Node::FunctionTerm(FunctionNode {
            bind: "x".to_string(),
            body: Box::new(PolyML4Node::IfTerm(IfNode {
                cond: Box::new(PolyML4Node::OpTerm(OpNode {
                    lhs: Box::new(PolyML4Node::Variable(VariableNode("x".to_string()))),
                    op: Op::Lt,
                    rhs: Box::new(PolyML4Node::Integer(IntegerNode(1))),
                })),
                t_branch: Box::new(PolyML4Node::Variable(VariableNode("x".to_string()))),
                f_branch: Box::new(PolyML4Node::OpTerm(OpNode {
                    lhs: Box::new(PolyML4Node::Variable(VariableNode("x".to_string()))),
                    op: Op::Plus,
                    rhs: Box::new(PolyML4Node::Integer(IntegerNode(1))),
                })),
            })),
        })),
        expr_2: Box::new(PolyML4Node::ApplicationTerm(ApplicationNode {
            f: Box::new(PolyML4Node::Variable(VariableNode("gt".to_string()))),
            p: Box::new(PolyML4Node::Integer(IntegerNode(1))),
        })),
    });

    let mut visitor = TypeCheckVisitor::new(TypedEnv::new(), Some(MonoType::Integer));

    let (tree, _) = term.apply_visitor(&mut visitor).unwrap();

    let mut print_visitor = PrintVisitor::new(0);
    tree.apply_visitor(&mut print_visitor).unwrap();
    let result = print_visitor.buffer.format(2);
    print!("{}", result);
}
