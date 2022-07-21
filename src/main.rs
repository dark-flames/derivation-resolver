#![feature(box_patterns)]

#[macro_use]
extern crate pest_derive;
extern crate pest;

mod error;
mod interface;
mod print;
mod systems;
mod utils;

use crate::interface::System;
use crate::print::ToToken;
use std::env::args;
use std::fs::read_to_string;
use systems::DerivationSystem;

fn main() {
    let mut args = args();
    args.next();
    let file = args.next().expect("No input file").to_string();
    let source = read_to_string(file.as_str()).expect("Unable to read the input file");

    let buffer = DerivationSystem::derive(source.as_str()).and_then(|t| t.token_buffer(0));

    match buffer {
        Ok(b) => print!("{}", b.format(2)),
        Err(e) => eprintln!("{}", e),
    };
}

#[test]
fn test() {
    use interface::System;
    use print::ToToken;
    use systems::eval_ml_3::EvalML3;

    let buffer = EvalML3::derive(
        "|- let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2 evalto 65536",
    )
    .and_then(|t| t.token_buffer(0));

    if let Err(e) = &buffer {
        println!("{}", e.to_string())
    }

    print!("{}", buffer.unwrap().format(2));
}
