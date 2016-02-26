use std::env;

pub mod util;
pub mod parser;

fn parse_arguments() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        println!("The first argument is {}", args[1]);
        match args[1].parse::<i32>() {
            Ok(n) => println!("{} is a valid number!", n),
            Err(_) => println!("The first argument is not a number."),
        };
        let i = args[1].parse::<i32>().unwrap_or(0);
        println!("Parsed first argument is {}", i);
    }
    // println!("{}", is_double("5.0"));
}

fn main() {
    parse_arguments();
}
