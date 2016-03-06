use std::str::Chars;
use std::rc::Rc;

use util::is_any_of::*;
use parser::util::char_util::CharProperties;
use parser::token::*;
use parser::token_stream::TokenStream;
use parser::compile_context::CompileContext;

// This is the lexer implementation for the parser (that sadly doesn't exist yet).
// I am fully aware of super-neat tools that can generate lexers and parser automatically,
// however, I want to implement this to learn about Rust and because I also want full control
// over things in my code base.
// Keep in mind that this implementation isn't final as many things like scan_string_literal(...)
// are still missing or are not completely implemented, yet (like scan_char_literal(...)).

pub struct Lexer<'input, 'ctx> {
	context: &'ctx CompileContext,
	input: Chars<'input>,
	buffer: String,
	cur_char: char
}

impl<'input, 'ctx> Lexer<'input, 'ctx> {
	pub fn new(
		ctx: &'ctx CompileContext,
		iterator: Chars<'input>
	)
		-> Lexer<'input, 'ctx>
	{
		let mut lexer = Lexer {
			context: ctx,
			input: iterator,
			buffer: String::new(),
			cur_char: '\0' };
		lexer.consume();
		lexer
	}

	pub fn new_from_str<'content: 'input>(
		ctx: &'ctx CompileContext,
		content : &'content str
	)
		-> Lexer<'input, 'ctx>
	{
		Lexer::new(ctx, content.chars())
	}

	/// Consumes the next character unwraped and
	/// returns reference to self for method chaining
	fn consume(&mut self) -> &mut Self {
		self.buffer.push(self.cur_char);
		self.cur_char = self.input.next().unwrap_or('\0');
		self
	}

	/// Returns the char from input which was read last
	fn get(&self) -> char {
		self.cur_char
	}

	/// Returns the given token, used as helper method
	/// for method chaining in order to improve the code-flow
	/// May be more important in future versions for managing
	/// of source locations.
	fn make(&self, token: Token) -> Token {
		token
	}

	/// Clears all chars in the buffer for special tokens
	/// and returns reference to self for method chaining
	fn clear_buffer(&mut self) -> &mut Self {
		self.buffer.clear();
		self
	}

	/// Drains the content of this buffer by performing
	/// a trial insertion at the context's StringTable.
	/// This buffer is empty after this operation!
	fn drain_buffer(&mut self) -> Rc<String> {
		let rc = self.context.get_string_table().get_or_insert(&self.buffer);
		self.clear_buffer();
		rc
	}

	fn scan_line_comment(&mut self) -> Token {
		assert_eq!(self.get(), '/');
		self.take_while(|c| c.is_none_of(&['\n','\0']));
		self.consume();
		self.make(Token::Comment)
	}

	fn scan_multi_line_comment(&mut self) -> Token {
		assert_eq!(self.get(), '*');
		self.consume();
		loop {
			match self.get() {
				'*' => match self.consume().get() {
					'/' => return self.consume().make(Token::Comment),
					'*' => continue,
					_   => self.consume()
				},
				'\0' => return self.make(Token::Error),
				_ => self.consume()
			};
		}
	}

	fn scan_identifier(&mut self) -> Token {
		assert!(self.get().is_alpha());
		self.take_while(|c| c.is_alpha_numeral() || c == '_');
		let drained = self.drain_buffer();
		self.make(Token::Identifier(drained))

		// omg this doesn't work because the borrow-checker
		// can not handle situations like this properly at the moment
		// self.make(Token::Identifier(self.drain_buffer()))
	}

	fn scan_char_suffix(&mut self) -> Token {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Char;
		assert_eq!(self.get(), '\'');
		match self.consume().get() {
			c if c.is_alpha() => {
				self.take_while(|c| c.is_alpha_numeral() || c == '_');
				let drained = self.drain_buffer();
				self.make(Literal(Char(drained)))
			},
			_ => {
				let drained = self.drain_buffer();
				self.make(Literal(Char(drained)))
			}
		}
	}

	// Accepts closed char sequences and forwards to the suffix scanning routine.
	// e.g. 'a'.
	fn scan_char_closure(&mut self) -> Token {
		use parser::token::Token::Error;
		match self.get() {
			'\'' => self.scan_char_suffix(),
			_    => self.make(Error) // error: expected a ' to close char literal!
		}
	}

	// Accepts char sequences for short-unicode annotation.
	// e.g. '\x7F'
	fn scan_char_ascii_hexcode(&mut self) -> Token {
		use parser::token::Token::Error;
		assert_eq!(self.get(), 'x');
		match self.consume().get() {
			/* valid unicode starting code-point */
			'0' ... '7' => match self.consume().get() {
				/* valid unicode 2nd code-point given */
				'0' ... '9' |
				'A' ... 'F' => self.consume().scan_char_closure(),

				/* error: requires upper-case alphas */
				'a' ... 'z' => self.make(Error),

				/* error: just one unicode code-point given */
				'\'' => self.make(Error),

				/* error: invalid 2nd code-point */
				_ => self.make(Error)
			},

			/* error: no hex-digits provided */
			'\'' => self.make(Error),

			/* error: requires upper-case alphas */
			'a' ... 'z' => self.make(Error),

			/* error: invalid starting points */
			'8' ... '9' |
			'A' ... 'F' => self.make(Error),

			/* anything else invalid */
			_ => self.make(Error)
		}
	}

	// Accepts char sequences for long-unicode annotation.
	// e.g. '\u{7FFFFF}'
	fn scan_char_unicode(&mut self) -> Token {
		assert_eq!(self.get(), 'u');
		// TODO ...
		self.scan_char_closure()
	}

	// Accepts char sequences with escape sequences as content.
	// e.g. '\n', '\\', '\'', '\x7F' or '\u{7FFFFF}', etc.
	fn scan_char_escape_sequence(&mut self) -> Token {
		use parser::token::Token::Error;
		assert_eq!(self.get(), '\\');
		match self.consume().get() {
			'0'  | /* Null */
			'n'  | /* LineFeed */
			'r'  | /* CarryReturn */
			't'  | /* Tab */
			'\\'   /* BackSlash */ => self.consume().scan_char_closure(),

			'x' => self.scan_char_ascii_hexcode(),
			'u' => self.scan_char_unicode(),
			'\'' => self.make(Error), // error: empty char literal!
			_ => self.make(Error) // error: unknown escape sequence!
		}
	}

	fn scan_char_literal(&mut self) -> Token {
		use std::ascii::AsciiExt;
		use parser::token::Token::*;
		assert_eq!(self.get(), '\'');
		match self.consume().get() {
			'\\' => self.scan_char_escape_sequence(),
			c if c.is_ascii() => self.consume().scan_char_closure(),
			_ => self.make(Error) // error: no valid ascii!
		}
	}

	fn scan_string_literal(&mut self) -> Token {
		self.make(Token::Error)
	}

	fn is_followed_by_min<C, U>(&mut self, n: usize, pred_counted: C, pred_uncounted: U) -> bool
		where C: Fn(char) -> bool,
		      U: Fn(char) -> bool
	{
		let mut count_valid = 0usize;
		loop {
			if pred_counted(self.get()) {
				count_valid += 1;
				self.consume();
			}
			else if pred_uncounted(self.get()) {
				self.consume();
			}
			else {
				break;
			}
		}
		count_valid >= n
	}

	// fn scan_signed_integer_suffix(&mut self) -> Token {
	// 	assert_eq!(self.get(), 'i');
	// 	match self.consume().peek_four() {
	// 		('8',  _ ,  _ ,  _ ) => self.consume_n(1).make(Literal(Integer(I8))),
	// 		('1', '6',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(I16))),
	// 		('3', '2',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(I32))),
	// 		('6', '4',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(I64))),
	// 		('s', 'i', 'z', 'e') => self.consume_n(4).make(Literal(Integer(ISize))),
	// 		_ => Token::Error
	// 	}
	// }

	// fn scan_unsigned_integer_suffix(&mut self) -> Token {
	// 	assert_eq!(self.get(), 'u');
	// 	match self.consume().peek_four() {
	// 		('8',  _ ,  _ ,  _ ) => self.consume_n(1).make(Literal(Integer(U8))),
	// 		('1', '6',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(U16))),
	// 		('3', '2',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(U32))),
	// 		('6', '4',  _ ,  _ ) => self.consume_n(2).make(Literal(Integer(U64))),
	// 		('s', 'i', 'z', 'e') => self.consume_n(4).make(Literal(Integer(USize))),
	// 		_ => Token::Error
	// 	}
	// }

	// fn scan_float_suffix(&mut self) -> Token {
	// 	assert_eq!(self.get(), 'f');
	// 	match self.consume().peek_two() {
	// 		('3', '2') => self.consume_n(2).make(Literal(Float(F32))),
	// 		('6', '4') => self.consume_n(2).make(Literal(Float(F64))),
	// 		_ => Token::Error
	// 	}
	// }

	fn scan_integer_suffix(&mut self) -> Token {
		// if self.get() == '\'' {
		// 	match self.consume().get() {
		// 		'i' => self.scan_signed_integer_suffix(),
		// 		'u' => self.scan_unsigned_integer_suffix(),
		// 		'f' => self.scan_float_suffix(),
		// 		 _  => {
		// 			let drained = self.drain_buffer();
		// 			self.make(Token::Literal(LiteralToken::Integer(drained)))
		// 		 }
		// 	}
		// } else {
		// 	let drained = self.drain_buffer();
		// 	self.make(Token::Literal(LiteralToken::Integer(drained)))
		// }
		let drained = self.drain_buffer();
		self.make(Token::Literal(LiteralToken::Integer(drained)))
	}

	fn scan_binary_literal(&mut self) -> Token {
		assert_eq!(self.get(), 'b');
		self.consume();
		if self.is_followed_by_min(1, |c| c.is_binary_numeral(), |c| c == '_') {
			self.take_while(|c| c.is_binary_numeral() || c == '_');
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_octal_literal(&mut self) -> Token {
		assert_eq!(self.get(), 'o');
		self.consume();
		if self.is_followed_by_min(1, |c| c.is_octal_numeral(), |c| c == '_') {
			self.take_while(|c| c.is_octal_numeral() || c == '_');
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_hexdec_literal(&mut self) -> Token {
		assert_eq!(self.get(), 'x');
		self.consume();
		if self.is_followed_by_min(1, |c| c.is_hexdec_numeral(), |c| c == '_') {
			self.take_while(|c| c.is_hexdec_numeral() || c == '_');
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_decimal_literal(&mut self) -> Token {
		assert!(self.get().is_decimal_numeral() || self.get() == '_');
		self.take_while(|c| c.is_decimal_numeral() || c == '_');
		match self.get() {
			'.' => self.scan_float_literal(),
			_ => {
				let drained = self.drain_buffer();
				self.make(Token::Literal(
					LiteralToken::Integer(drained)))
			}
		}
	}

	fn scan_float_literal_exponent(&mut self) -> Token {
		match self.get() {
			'e' => match self.consume().get() {
				'+' | '-' => match self.consume().get() {
					c if c.is_decimal_numeral() => {
						self.take_while(|c| c.is_decimal_numeral());
						let drained = self.drain_buffer();
						self.make(Token::Literal(
							LiteralToken::Float(drained)))
					},
					_ => self.make(Token::Error)
				},
				_ => {
					self.make(Token::Error)
				}
			},
			_ => {
				let drained = self.drain_buffer();
					self.make(Token::Literal(
						LiteralToken::Float(drained)))
			}
		}
	}

	fn scan_float_literal(&mut self) -> Token {
		assert_eq!(self.get(), '.');
		if self.consume().get().is_decimal_numeral() {
			self.take_while(|c| c.is_decimal_numeral() || c == '_');
			self.scan_float_literal_exponent()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_number_literal(&mut self) -> Token {
		assert!(self.get().is_decimal_numeral());
		match self.get() {
			'0' => match self.consume().get() {
				'b' => self.scan_binary_literal(),
				'o' => self.scan_octal_literal(),
				'x' => self.scan_hexdec_literal(),
				'.' => self.scan_float_literal(),

				/* dec number literals cannot start with '0' */
				'0' ... '9' | '_' => self.scan_decimal_literal(),

				/* literal number suffix, e.g. 0i32 */
				c if c.is_alpha() => self.make(Token::Error),

				/* */
				_ => self.make(Token::Error)
			},

			/* decimal number literal */
			'1' ... '9' => self.scan_decimal_literal(),

			_ => unreachable!()
		}
	}

	/// Take all characters from input as long as they fullfill the given predicate
	/// and returns reference to self for method chaining
	fn take_while<P>(&mut self, predicate: P) -> &mut Self
		where P: Fn(char) -> bool
	{
		while predicate(self.get()) {
			self.consume();
		}
		self
	}
}

impl<'input, 'ctx> TokenStream for Lexer<'input, 'ctx> {
	fn next_token(&mut self) -> Token {
		use parser::token::Token::*;
		use parser::token::DelimitToken::*;
		use parser::token::BinOpToken::*;
		use parser::token::LogicalOpToken::*;
		use parser::token::RelOpToken::*;
		self.clear_buffer();
		match self.get() {
			/* Skip whitespace */
			c if c.is_whitespace() => {
				self.take_while(|c| c.is_whitespace());
				self.make(Whitespace)
			},

			/* Opening delimiters */
			'(' => self.consume().make(OpenDelim(Paren)),
			'[' => self.consume().make(OpenDelim(Bracket)),
			'{' => self.consume().make(OpenDelim(Brace)),

			/* Opening delimiters */
			')' => self.consume().make(CloseDelim(Paren)),
			']' => self.consume().make(CloseDelim(Bracket)),
			'}' => self.consume().make(CloseDelim(Brace)),

			/* Special tokens which aren't the beginning
			   of any other token */
			'?' => self.consume().make(Question),
			';' => self.consume().make(SemiColon),
			',' => self.consume().make(Comma),
			'_' => self.consume().make(Underscore),

			/* Experimental syntax with the 'peek' construct */
			// '.' => match self.peek_three() {
			// 	('.','.','.') => self.consume_n(3).make(DotDotDot),
			// 	('.','.', _ ) => self.consume_n(2).make(DotDot),
			// 	_ => self.consume().make(Dot),
			// },

			/* Dot, DotDot and DotDotDot tokens */
			'.' => match self.consume().get() {
				'.' => match self.consume().get() {
					'.' => self.consume().make(DotDotDot),
					_   => self.make(DotDot)
				},
				_ => self.make(Dot)
			},

			/* Tokens starting with '+' */
			'+' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Plus)),
				_   => self.make(BinOp(Plus))
			},

			/* Tokens starting with '-' */
			'-' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Minus)),
				'>' => self.consume().make(Arrow),
				_   => self.make(BinOp(Minus))
			},

			/* Tokens starting with '*' */
			'*' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Star)),
				_   => self.make(BinOp(Star))
			},

			/* Tokens starting with '/' */
			'/' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Slash)),
				'/' => self.scan_line_comment(),
				'*' => self.scan_multi_line_comment(),
				_ => self.make(BinOp(Slash))
			},

			/* Tokens starting with '%' */
			'%' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Percent)),
				_   => self.make(BinOp(Percent))
			},

			/* Tokens starting with '^' */
			'^' => match self.consume().get() {
				'=' => self.consume().make(BinOpEq(Caret)),
				_   => self.make(BinOp(Caret))
			},

			/* Tokens starting with '!' */
			'!' => match self.consume().get() {
				'=' => self.consume().make(RelOp(NotEq)),
				_   => self.make(Exclamation)
			},

			/* Tokens starting with '=' */
			'=' => match self.consume().get() {
				'>' => self.consume().make(FatArrow),
				'=' => self.consume().make(RelOp(EqEq)),
				_   => self.make(Eq)
			},

			/* Tokens starting with '&' */
			'&' => match self.consume().get() {
				'&' => self.consume().make(LogicalOp(AndAnd)),
				'=' => self.consume().make(BinOpEq(And)),
				_   => self.make(BinOp(And))
			},

			/* Tokens starting with '|' */
			'|' => match self.consume().get() {
				'|' => self.consume().make(LogicalOp(OrOr)),
				'=' => self.consume().make(BinOpEq(Or)),
				_   => self.make(BinOp(Or))
			},

			/* Experimental syntax with the peek() construct */
			// '<' => match self.peek_three() {
			// 	('<', '<', '=') => self.consume_n(3).make(BinOpEq(Shl)),
			// 	('<', '<',  _ ) => self.consume_n(2).make(BinOp(Shl)),
			// 	('<', '=',  _ ) => self.consume_n(2).make(RelOp(LessEq)),
			// 	('<',  _ ,  _ ) => self.consume_n(1).make(RelOp(LessThan))
			// },

			/* Tokens starting with '<' */
			'<' => match self.consume().get() {
				'<' => match self.consume().get() {
					'=' => self.consume().make(BinOpEq(Shl)),
					_   => self.make(BinOp(Shl))
				},
				'=' => self.consume().make(RelOp(LessEq)),
				_   => self.make(RelOp(LessThan))
			},

			/* Tokens starting with '>' */
			'>' => match self.consume().get() {
				'>' => match self.consume().get() {
					'=' => self.consume().make(BinOpEq(Shr)),
					_   => self.make(BinOp(Shr))
				},
				'=' => self.consume().make(RelOp(GreaterEq)),
				_   => self.make(RelOp(GreaterThan))
			},

			/* Char and string literals */
			'\'' => self.scan_char_literal(),
			'\"' => self.scan_string_literal(),

			/* Integer- and float literals and identifiers */
			c if c.is_decimal_numeral() => self.scan_number_literal(),

			/* Identifiers and keywords */
			c if c.is_alpha() => self.scan_identifier(),

			/* When end of iterator has been reached */
			_ => self.make(EndOfFile)
		}
	}
}

impl<'input, 'ctx> Iterator for Lexer<'input, 'ctx> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next_token();
		match token {
			Token::EndOfFile => None,
			_                => Some(token)
		}
	}
}

#[cfg(test)]
mod tests {
	use parser::lexer::*;
	use parser::token::*;
	use parser::compile_context::CompileContext;

	#[test]
	fn simple_tokens() {
		use parser::token::Token::*;
		use parser::token::DelimitToken::*;
		let solution = vec![
			OpenDelim(Paren), CloseDelim(Paren),
			OpenDelim(Bracket), CloseDelim(Bracket),
			OpenDelim(Brace), CloseDelim(Brace),

			Question,
			SemiColon,
			Comma
		];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx, "()[]{}?;,");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_comments() {
		let solution = vec![
			Token::Comment,
			Token::Whitespace,
			Token::Comment,
			Token::Whitespace,
			Token::Comment
		];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx,
			"//Hello, World!
			/*Ignored new line!\nBlub!\nSee?*/
			/****multiple stars don't hurt****/");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_dots() {
		let solution = vec![
			Token::DotDot,
			Token::Whitespace,
			Token::DotDotDot,
			Token::Comment,
			Token::Dot
		];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx, "..\t.../*Useless comment*/.");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_char_literal() {
		let solution = vec![
			Token::char_literal_from_str(r"c"),
			Token::Whitespace,
			Token::char_literal_from_str(r"\n"),
			Token::Whitespace,
			Token::char_literal_from_str(r"\t"),
			Token::Whitespace,
			Token::char_literal_from_str(r"\x7F")
		];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx, r"'c' '\n' '\t' '\x7F'");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_whitespace() {
		let solution = vec![Token::Whitespace];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx, " \t\r\n");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_integral_literals() {
		use parser::token::Token::*;
		let solution = vec![
			Token::integer_literal_from_str("0b1011_0010_0000_0001"),
			Whitespace,
			Token::integer_literal_from_str("0o731_312_645_003"),
			Whitespace,
			Token::integer_literal_from_str("0xFF_AE_03_95"),
			Whitespace,
			Token::integer_literal_from_str("987654321"),
			Whitespace,
			Error,
			Whitespace,
			Error,
			Whitespace,
			Error,
			Whitespace,
			Token::integer_literal_from_str("0__")
		];
		let ctx   = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx,
			"0b1011_0010_0000_0001
			 0o731_312_645_003
			 0xFF_AE_03_95
			 987654321
			 0b_
			 0o___
			 0x_____
			 0__");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_float_literals() {
		let solution = vec![
			Token::float_literal_from_str("0.0"),
			Token::Whitespace,
			Token::float_literal_from_str("42.0"),
			Token::Whitespace,
			Token::float_literal_from_str("0.24"),
			Token::Whitespace,
			Token::float_literal_from_str("13.37"),
			Token::Whitespace,
			Token::float_literal_from_str("0.00001"),
			Token::Whitespace,
			Token::float_literal_from_str("1.23e+12"),
			Token::Whitespace,
			Token::float_literal_from_str("0.01e+07"),
			Token::Whitespace,
			Token::float_literal_from_str("1_.1_"),
			Token::EndOfFile
		];
		let ctx = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx,
			"0.0
			 42.0
			 0.24
			 13.37
			 0.00001
			 1.23e+12
			 0.01e+07
			 1_.1_");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}

	#[test]
	fn simple_identifiers() {
		let solution = vec![
			Token::identifier_from_str("true"),
			Token::Whitespace,
			Token::identifier_from_str("false"),
			Token::Whitespace,
			Token::identifier_from_str("alphanumeric"),
			Token::Whitespace,
			Token::identifier_from_str("with_underscore"),
			Token::Whitespace,
			Token::identifier_from_str("underscores_at_the_end__"),
			Token::Whitespace,
			Token::identifier_from_str("with_n0m3r5")
		];
		let ctx = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx,
			"true false
			 alphanumeric
			 with_underscore
			 underscores_at_the_end__
			 with_n0m3r5");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}
}
