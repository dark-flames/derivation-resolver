#![feature(box_patterns)]

#[macro_use]
extern crate pest_derive;
extern crate pest;

use crate::print::{ToToken, TokenBuffer};

mod error;
mod interface;
mod print;
mod systems;

fn main() {
    println!("Hello, world!");
}

#[test]
fn test() {
    use systems::eval_ml_3::eval::eval;
    use systems::eval_ml_3::syntax::*;

    let env = Env::Terminal;
    let node = AstNode::LetInTerm {
        ident: "sq".to_string(),
        expr_1: Box::new(AstNode::Function {
            bind: "x".to_string(),
            body: Box::new(AstNode::Op {
                lhs: Box::new(AstNode::Variable("x".to_string())),
                op: Op::Times,
                rhs: Box::new(AstNode::Variable("x".to_string())),
            }),
        }),
        expr_2: Box::new(AstNode::Op {
            lhs: Box::new(AstNode::Application {
                f: Box::new(AstNode::Variable("sq".to_string())),
                p: Box::new(AstNode::Integer(3)),
            }),
            op: Op::Plus,
            rhs: Box::new(AstNode::Application {
                f: Box::new(AstNode::Variable("sq".to_string())),
                p: Box::new(AstNode::Integer(4)),
            }),
        }),
    };

    let (derivation_tree, _) = eval(&env, &node).unwrap();
    let mut buffer = TokenBuffer::default();
    derivation_tree.to_token(&mut buffer).unwrap();
    print!("{}", buffer.format(2));
}
