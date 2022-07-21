#![feature(box_patterns)]

#[macro_use]
extern crate pest_derive;
extern crate pest;

use crate::print::ToToken;

mod error;
mod interface;
mod print;
mod systems;
mod utils;

fn main() {
    println!("Hello, world!");
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
