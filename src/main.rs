use std::env;

pub mod util;
pub mod parser;

fn parse_arguments() -> Result<i32, String> {
    let args = env::args().skip(1).next();

    match args {
        Some(s) => Ok(s.parse().unwrap_or(0)),
        None => Err(String::from("Not enough arguments")),
    }
}

use std::mem;
use std::rc::Rc;

fn main() {
    let arg = match parse_arguments() {
        Ok(arg) => arg,
        Err(e) => panic!("There was an error parsing arguments: {}", e),
    };
    println!("Parsed first argument is {}", arg);
}
