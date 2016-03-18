use std::str::Chars;
use std::collections::VecDeque;

use util::is_any_of::*;
use parser::util::char_util::CharProperties;
use parser::token::*;
use parser::compile_context::CompileContext;
use parser::string_cache::Name;

// This is the lexer implementation for the parser (that sadly doesn't exist yet).
//
// This implementation is still experimental!
// Many things like scan_string_literal(...) are still missing or are not implemented at all.

pub struct Lexer<'input, 'ctx> {
	context: &'ctx CompileContext,
	input: Chars<'input>,
	buffer: String,
	peeked: VecDeque<char>
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
			peeked: VecDeque::new()
		};
		lexer.pull();
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

	fn pull(&mut self) -> &mut Self {
		let pulled = self.input.next().unwrap_or('\0');
		self.peeked.push_back(pulled);
		self
	}

	fn consume(&mut self) -> &mut Self {
		assert!(!self.peeked.is_empty());
		let consumed = self.peeked.pop_front().unwrap();
		self.buffer.push(consumed);
		self
	}

	fn consume_n(&mut self, n: usize) -> &mut Self {
		assert!(n <= self.peeked.len());
		for _ in 0..n {
			self.consume();
		}
		self
	}

	fn peek(&mut self) -> char {
		if self.peeked.is_empty() {
			self.pull();
		}
		self.peeked[0]
	}

	fn peek_2(&mut self) -> (char, char) {
		use std::cmp;
		let pulls_required = cmp::max(0, (2 - (self.peeked.len() as isize)));
		for _ in 0..pulls_required {
			self.pull();
		}
		(self.peeked[0], self.peeked[1])
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
	fn drain_buffer(&mut self) -> Name {
		let name = self.context.string_cache.borrow_mut().intern(&self.buffer);
		self.clear_buffer();
		name
	}

	fn scan_line_comment(&mut self) -> Token {
		assert_eq!(self.peek(), '/');
		self.take_while(|c| c.is_none_of(&['\n','\0']));
		self.consume();
		self.make(Token::Comment)
	}

	fn scan_multi_line_comment(&mut self) -> Token {
		assert_eq!(self.peek(), '*');
		self.consume();
		loop {
			match self.peek() {
				'*' => match self.consume().peek() {
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
		assert!(self.peek().is_alpha());
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
		assert_eq!(self.peek(), '\'');
		match self.consume().peek() {
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
		match self.peek() {
			'\'' => self.scan_char_suffix(),
			_    => self.make(Error) // error: expected a ' to close char literal!
		}
	}

	// Accepts char sequences for short-unicode annotation.
	// e.g. '\x7F'
	fn scan_char_ascii_hexcode(&mut self) -> Token {
		use parser::token::Token::Error;
		assert_eq!(self.peek(), 'x');
		match self.consume().peek() {
			/* valid unicode starting code-point */
			'0' ... '7' => match self.consume().peek() {
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
		assert_eq!(self.peek(), 'u');
		// TODO ...
		self.scan_char_closure()
	}

	// Accepts char sequences with escape sequences as content.
	// e.g. '\n', '\\', '\'', '\x7F' or '\u{7FFFFF}', etc.
	fn scan_char_escape_sequence(&mut self) -> Token {
		use parser::token::Token::Error;
		assert_eq!(self.peek(), '\\');
		match self.consume().peek() {
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
		assert_eq!(self.peek(), '\'');
		match self.consume().peek() {
			'\\' => self.scan_char_escape_sequence(),
			c if c.is_ascii() => self.consume().scan_char_closure(),
			_ => self.make(Error) // error: no valid ascii!
		}
	}

	fn scan_string_literal(&mut self) -> Token {
		self.make(Token::Error) // TODO
	}

	fn scan_integer_and_float_suffix(&mut self) -> &mut Self {
		if self.peek() == '\'' {
			if self.consume().peek().is_alpha() {
				self.take_while(|c| c.is_alpha_numeral() || c == '_');
			}
			else {
				// TODO: error!
			}
		}
		self
	}

	fn scan_float_suffix(&mut self) -> Token {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Float;
		self.scan_integer_and_float_suffix();
		let drained = self.drain_buffer();
		self.make(Literal(Float(drained)))
	}

	fn scan_integer_suffix(&mut self) -> Token {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Integer;
		self.scan_integer_and_float_suffix();
		let drained = self.drain_buffer();
		self.make(Literal(Integer(drained)))
	}

	fn scan_binary_literal(&mut self) -> Token {
		assert_eq!(self.peek(), 'b');
		self.consume();
		if 1 <= self.count_follow_by(|c| c.is_binary_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_octal_literal(&mut self) -> Token {
		assert_eq!(self.peek(), 'o');
		self.consume();
		if 1 <= self.count_follow_by(|c| c.is_octal_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_hexdec_literal(&mut self) -> Token {
		assert_eq!(self.peek(), 'x');
		self.consume();
		if 1 <= self.count_follow_by(|c| c.is_hexdec_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_decimal_literal(&mut self) -> Token {
		assert!(self.peek().is_decimal_numeral() || self.peek() == '_');
		self.take_while(|c| c.is_decimal_numeral() || c == '_');
		match self.peek() {
			'.' => self.scan_possible_float_literal(),
			_ => self.scan_integer_suffix()
		}
	}

	fn scan_float_literal_exponent(&mut self) -> Token {
		match self.peek() {
			'e' => match self.consume().peek() {
				'+' | '-' => match self.consume().peek() {
					c if c.is_decimal_numeral() => {
						self.take_while(|c| c.is_decimal_numeral());
						self.scan_float_suffix()
					},
					_ => self.make(Token::Error)
				},
				_ => {
					self.make(Token::Error)
				}
			},
			_ => {
				self.scan_float_suffix()
			}
		}
	}

	fn scan_float_literal(&mut self) -> Token {
		let ( p, c ) = self.peek_2();
		assert!(p == '.' && c.is_decimal_numeral());
		self.consume_n(2);
		self.take_while(|c| c.is_decimal_numeral() || c == '_');
		self.scan_float_literal_exponent()
	}

	/// Be careful, a '.' following a sequence of digits doesn't
	/// nessecarily mean that the following characters are part of
	/// a Float literal.
	/// E.g. a method call on an integer literal may be possible, too:
	///    42.foo()
	/// Or a range expression:
	///    0..10
	fn scan_possible_float_literal(&mut self) -> Token {
		assert_eq!(self.peek(), '.');
		match self.peek_2() {
			('.',  c ) if c.is_decimal_numeral() => self.scan_float_literal(),
			_ => {
				let drained = self.drain_buffer();
				self.make(Token::Literal(LiteralToken::Integer(drained)))
			}
		}
	}

	fn scan_number_literal(&mut self) -> Token {
		assert!(self.peek().is_decimal_numeral());
		match self.peek() {
			'0' => match self.consume().peek() {
				'b' => self.scan_binary_literal(),
				'o' => self.scan_octal_literal(),
				'x' => self.scan_hexdec_literal(),
				'.' => self.scan_float_literal(),

				/* decimal number literal */
				'0' ... '9' | '_' => self.scan_decimal_literal(),

				/* just a null (0) */
				_ => self.scan_integer_suffix()
			},

			/* decimal number literal */
			_ => self.scan_decimal_literal(),
		}
	}

	fn count_follow_by<C, U>(&mut self, pred_counted: C, pred_uncounted: U) -> usize
		where C: Fn(char) -> bool,
		      U: Fn(char) -> bool
	{
		let mut count_valid = 0usize;
		loop {
			if pred_counted(self.peek()) {
				count_valid += 1;
				self.consume();
			}
			else if pred_uncounted(self.peek()) {
				self.consume();
			}
			else {
				break;
			}
		}
		count_valid
	}

	/// Take all characters from input as long as they fullfill the given predicate
	/// and returns reference to self for method chaining
	fn take_while<P>(&mut self, predicate: P) -> &mut Self
		where P: Fn(char) -> bool
	{
		while predicate(self.peek()) {
			self.consume();
		}
		self
	}
}

// Types like the Lexer implement this trait as they are iterators
// over Tokens. Maybe it should be substituted simply with the Iterator<Token> trait.
// However, a next_token(...) method is more explicit than just next(...) and 
// it allows one to add new required methods (e.g. peek_token(...)) in future versions.
pub trait TokenStream {
    fn next_token(&mut self) -> Token;
}

impl<'input, 'ctx> TokenStream for Lexer<'input, 'ctx> {
	fn next_token(&mut self) -> Token {
		use parser::token::Token::*;
		use parser::token::DelimitToken::*;
		use parser::token::BinOpToken::*;
		use parser::token::LogicalOpToken::*;
		use parser::token::RelOpToken::*;
		self.clear_buffer();
		match self.peek() {
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

			/* Dot, DotDot and DotDotDot tokens */
			'.' => match self.consume().peek_2() {
				('.', '.') => self.consume_n(2).make(DotDotDot),
				('.',  _ ) => self.consume().make(DotDot),
				( _ ,  _ ) => self.make(Dot)
			},

			/* Tokens starting with '+' */
			'+' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Plus)),
				_   => self.make(BinOp(Plus))
			},

			/* Tokens starting with '-' */
			'-' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Minus)),
				'>' => self.consume().make(Arrow),
				_   => self.make(BinOp(Minus))
			},

			/* Tokens starting with '*' */
			'*' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Star)),
				_   => self.make(BinOp(Star))
			},

			/* Tokens starting with '/' */
			'/' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Slash)),
				'/' => self.scan_line_comment(),
				'*' => self.scan_multi_line_comment(),
				_ => self.make(BinOp(Slash))
			},

			/* Tokens starting with '%' */
			'%' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Percent)),
				_   => self.make(BinOp(Percent))
			},

			/* Tokens starting with '^' */
			'^' => match self.consume().peek() {
				'=' => self.consume().make(BinOpEq(Caret)),
				_   => self.make(BinOp(Caret))
			},

			/* Tokens starting with '!' */
			'!' => match self.consume().peek() {
				'=' => self.consume().make(RelOp(NotEq)),
				_   => self.make(Exclamation)
			},

			/* Tokens starting with '=' */
			'=' => match self.consume().peek() {
				'>' => self.consume().make(FatArrow),
				'=' => self.consume().make(RelOp(EqEq)),
				_   => self.make(Eq)
			},

			/* Tokens starting with '&' */
			'&' => match self.consume().peek() {
				'&' => self.consume().make(LogicalOp(AndAnd)),
				'=' => self.consume().make(BinOpEq(And)),
				_   => self.make(BinOp(And))
			},

			/* Tokens starting with '|' */
			'|' => match self.consume().peek() {
				'|' => self.consume().make(LogicalOp(OrOr)),
				'=' => self.consume().make(BinOpEq(Or)),
				_   => self.make(BinOp(Or))
			},

			/* Tokens starting with '<' */
			'<' => match self.consume().peek_2() {
				('<', '=') => self.consume_n(2).make(BinOpEq(Shl)),
				('<',  _ ) => self.consume().make(BinOp(Shl)),
				('=',  _ ) => self.consume().make(RelOp(LessEq)),
				( _ ,  _ ) => self.make(RelOp(LessThan))
			},

			/* Tokens starting with '>' */
			'>' => match self.consume().peek_2() {
				('>', '=') => self.consume_n(2).make(BinOpEq(Shr)),
				('>',  _ ) => self.consume().make(BinOp(Shr)),
				('=',  _ ) => self.consume().make(RelOp(GreaterEq)),
				( _ ,  _ ) => self.make(RelOp(GreaterThan))
			},

			/* Char and string literals */
			'\'' => self.scan_char_literal(),
			'"' => self.scan_string_literal(),

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
		use parser::token::Token::{Whitespace, Literal, EndOfFile};
		use parser::token::LiteralToken::Char;
		let ctx   = CompileContext::default();
		let mut lexer = Lexer::new_from_str(
			&ctx, r"'c' '\n' '\t' '\x7F' 'a's '\n'asd0");
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'c'"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'\n'"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'\t'"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'\x7F'"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'a's"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Char(sc.borrow_mut().intern(r"'\n'asd0"))));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_whitespace() {
		let ctx   = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx, " \t\r\n");
		assert_eq!(lexer.next_token(), Token::Whitespace);
		assert_eq!(lexer.next_token(), Token::EndOfFile);
	}

	#[test]
	fn simple_integral_literals() {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Integer;
		let ctx   = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx,
			"0b1011_0010_0000_0001
			 0o731_312_645_003
			 0xFF_AE_03_95
			 987654321
			 0b_
			 0o___
			 0x_____
			 0__");
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0b1011_0010_0000_0001"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0o731_312_645_003"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0xFF_AE_03_95"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("987654321"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(), Error);
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(), Error);
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(), Error);
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0__"))));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_integral_suffixes() {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Integer;
		let ctx = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx,
			"0b0000__0101__1111_0001'i32
			 0o123_456_777_000'u64
			 0xFA_01_DE_23'f32
			 1234567890'hello_word"
		);
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0b0000__0101__1111_0001'i32"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0o123_456_777_000'u64"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("0xFA_01_DE_23'f32"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Integer(sc.borrow_mut().intern("1234567890'hello_word"))));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_float_literals() {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Float;
		let ctx = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx,
			"0.0
			 42.0
			 0.24
			 13.37
			 0.00_00_1
			 1.23e+12
			 0.01e-07
			 1_.1_");
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("0.0"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("42.0"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("0.24"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("13.37"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("0.00_00_1"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("1.23e+12"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("0.01e-07"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("1_.1_"))));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_float_suffixes() {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Float;
		let ctx = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx,
			"0.0'f32
			 567.0'i32
			 9.9999'f64
			 1_2_3.3_2_1e+14'alpha
			 9.87654321e-0'b37a");
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("0.0'f32"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("567.0'i32"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("9.9999'f64"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("1_2_3.3_2_1e+14'alpha"))));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Literal(Float(sc.borrow_mut().intern("9.87654321e-0'b37a"))));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_identifiers() {
		use parser::token::Token::*;
		let ctx = CompileContext::default();
		let mut lexer = Lexer::new_from_str(&ctx,
			"true false
			 alphanumeric
			 with_underscore
			 underscores_at_the_end__
			 with_n0m3r5");
		let sc = &ctx.string_cache;
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("true")));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("false")));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("alphanumeric")));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("with_underscore")));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("underscores_at_the_end__")));
		assert_eq!(lexer.next_token(), Whitespace);
		assert_eq!(lexer.next_token(),
			Identifier(sc.borrow_mut().intern("with_n0m3r5")));
		assert_eq!(lexer.next_token(), EndOfFile);
	}

	#[test]
	fn simple_less_symbol() {
		use parser::token::Token::*;
		use parser::token::BinOpToken::*;
		use parser::token::RelOpToken::*;
		let solution: Vec<Token> = vec![
			BinOpEq(Shl),
			Whitespace,
			BinOp(Shl),
			Whitespace,
			RelOp(LessEq),
			Whitespace,
			RelOp(LessThan)
		];
		let ctx = CompileContext::default();
		let lexer = Lexer::new_from_str(&ctx, "<<= << <= <");
		for zipped in solution.into_iter().zip(lexer) {
			assert_eq!(zipped.0, zipped.1);
		}
	}
}
