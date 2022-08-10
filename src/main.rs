#![feature(box_patterns)]

#[macro_use]
extern crate pest_derive;
extern crate pest;

mod derive;
mod error;
mod print;
mod systems;
mod utils;
mod visitor;

#[cfg(any(feature = "eval-ml-3", feature = "eval-ml-4", feature = "poly-ml-4"))]
fn main() {
    use crate::derive::System;
    use crate::systems::common::print::PrintVisitor;
    use crate::visitor::Visitable;
    use std::env::args;
    use std::fs::read_to_string;
    use systems::DerivationSystem;
    let mut args = args();
    args.next();
    let file = args.next().expect("No input file").to_string();
    let source = read_to_string(file.as_str()).expect("Unable to read the input file");

    let buffer = DerivationSystem::derive(source.as_str()).and_then(|t| {
        let mut print_visitor = PrintVisitor::new(0);
        t.apply_visitor(&mut print_visitor)?;
        print_visitor.into()
    });

    match buffer {
        Ok(b) => print!("{}", b.format(2)),
        Err(e) => eprintln!("{}", e),
    };
}
