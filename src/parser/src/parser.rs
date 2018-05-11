// use std::collections::VecDeque;

use token::*;
use parse_sess::{ParseSess};
// use string_cache::Name;
// use code_map::{FileMap, FileMapIterator, CharAndPos, Span};

pub struct Parser {
	context: ParseSess,
	token_stream: Box<Iterator<Item=Token>>
}

impl Parser {
	pub fn new(sess: ParseSess, token_stream: Box<Iterator<Item=Token>>) -> Self {
		Parser{ context: sess, token_stream: token_stream }
	}

	// pub fn parse_module(&self) -> PResult<Module> {

	// }
}
