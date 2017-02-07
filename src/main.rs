extern crate clap;

extern crate cionc_parser;

use clap::{Arg, App, SubCommand};

use std::env;

fn main() {
	let matches = App::new("Cion Compiler")
		.version("0.1.0")
		.author("Robbepop <robbepop@web.de>")
		.about("The official compiler for the Cion programming language.")
		.arg(Arg::with_name("input")
			.short("i")
			.long("input")
			.help("Specifies the entry input source file for translation.")
			.value_name("FILE")
			.takes_value(true))
		.arg(Arg::with_name("output")
			.short("o")
			.long("output")
			.help("Specifies the output binary file path.")
			.value_name("PATH")
			.takes_value(true))
		.arg(Arg::with_name("verbosity")
			.long("verbosity")
			.help("Sets the level of verbosity.")
			.value_name("LEVEL")
			.takes_value(true))
		.get_matches();
}
