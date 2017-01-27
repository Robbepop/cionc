// use std::collections::VecDeque;

// use token::*;
use compile_context::{ParseSess};
// use string_cache::Name;
// use code_map::{FileMap, FileMapIterator, CharAndPos, Span};

pub struct Parser {
	context: ParseSess
}

impl Parser {
	pub fn new(sess: ParseSess) -> Self {
		Parser{
			context: sess
		}
	}

	// pub fn parse_module(&self) -> PResult<Module> {

	// }
}
