use std::collections::VecDeque;

use token::*;
use parse_sess::{ParseSess};
use string_interner::Name;
use code_map::{FileMap, FileMapIterator, CharAndPos, Span};
// use util::char_util::CharProperties;

// This is the lexer implementation for the parser (that sadly doesn't exist yet).

#[derive(PartialEq, Eq, Debug)]
pub struct TokenAndSpan {
	tok: Token,
	sp: Span
}

pub struct Lexer {
	context      : ParseSess,
	input        : FileMapIterator,
	peeked       : VecDeque<CharAndPos>,
	name_buffer  : String,
	span_consumed: Span
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ConsumeBehaviour {
	Dump,
	Keep
}
use self::ConsumeBehaviour::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SeparatorConfig {
	Separable,
	Contiguous
}
use self::SeparatorConfig::*;

impl Lexer {
	pub fn new_for_filemap(
		ctx: ParseSess,
		fm : &FileMap
	)
		-> Lexer
	{
		let mut lexer = Lexer {
			context: ctx,
			input: fm.iter(),
			peeked: VecDeque::new(),
			name_buffer: String::new(),
			span_consumed: Span::new(fm.span.lo, fm.span.lo)
		};
		lexer.pull();
		lexer
	}

 	//==================================================================
 	// Helper routines
 	//==================================================================

	fn pull(&mut self) -> &mut Self {
		let pulled = self.input.next().unwrap_or(CharAndPos::default());
		self.peeked.push_back(pulled);
		self
	}

	fn reset_span_consumed(&mut self) -> &mut Self {
		self.peek();
		let start_pos = self.peeked[0].pos;
		self.span_consumed = Span::new(start_pos, start_pos);
		self
	}

	fn consume(&mut self, behaviour: ConsumeBehaviour) -> &mut Self {
		assert!(!self.peeked.is_empty());
		match behaviour {
			Dump => {
				let CharAndPos {ch: _, pos} = self.peeked.pop_front().unwrap();
				self.span_consumed = self.span_consumed.extend(pos);
			},
			Keep => {
				let CharAndPos {ch, pos} = self.peeked.pop_front().unwrap();
				self.span_consumed = self.span_consumed.extend(pos);
				self.name_buffer.push(ch);
			}
		}
		self
	}

	fn consume_n(&mut self, behaviour: ConsumeBehaviour, n: usize) -> &mut Self {
		assert!(n <= self.peeked.len());
		for _ in 0..n {
			self.consume(behaviour);
		}
		self
	}

	/// Take all characters from input as long as they fullfill the given predicate
	/// and returns reference to self for method chaining
	fn consume_while<P>(
		&mut self,
		behaviour: ConsumeBehaviour,
		predicate: P
	) -> &mut Self
		where P: Fn(char) -> bool
	{
		while predicate(self.peek()) {
			self.consume(behaviour);
		}
		self
	}

	fn consume_counted<P>(
		&mut self,
		behaviour: ConsumeBehaviour,
		predicate: P
	) -> usize
		where P: Fn(char) -> bool
	{
		let mut count = 0;
		while predicate(self.peek()) {
			count += 1;
			self.consume(behaviour);
		}
		count
	}

	fn peek(&mut self) -> char {
		if self.peeked.is_empty() {
			self.pull();
		}
		self.peeked[0].ch
	}

	fn peek_2(&mut self) -> (char, char) {
		use std::cmp;
		let pulls_required = cmp::max(0, (2 - (self.peeked.len() as isize)));
		for _ in 0..pulls_required {
			self.pull();
		}
		(self.peeked[0].ch, self.peeked[1].ch)
	}

	fn peek_3(&mut self) -> (char, char, char) {
		use std::cmp;
		let pulls_required = cmp::max(0, (3 - (self.peeked.len() as isize)));
		for _ in 0..pulls_required {
			self.pull();
		}
		(self.peeked[0].ch, self.peeked[1].ch, self.peeked[2].ch)
	}

	fn expect_char(&mut self, behaviour: ConsumeBehaviour, ch: char) -> &mut Self {
		assert_eq!(self.peek(), ch);
		self.consume(behaviour);
		self
	}

	fn expect_by<P>(&mut self, behaviour: ConsumeBehaviour, pred: P) -> &mut Self
		where P: FnOnce(char) -> bool
	{
		assert!(pred(self.peek()));
		self.consume(behaviour);
		self
	}

	/// Returns the given token, used as helper method
	/// for method chaining in order to improve the code-flow
	/// May be more important in future versions for managing
	/// of source locations.
	fn make(&self, token: Token) -> TokenAndSpan {
		let tok_span = TokenAndSpan { tok: token, sp: self.span_consumed };
		tok_span
	}

	/// Clears all chars in the buffer for special tokens
	/// and returns reference to self for method chaining
	fn reset_name_buffer(&mut self) -> &mut Self {
		self.name_buffer.clear();
		self
	}

	fn fetch_name(&self) -> Name {
		assert_eq!(self.name_buffer.is_empty(), false);
		self.context.symbol_table.borrow_mut().intern(&self.name_buffer)
	}

 	//==================================================================
 	// Scanning routines
 	//==================================================================

	fn scan_line_comment(&mut self) -> TokenAndSpan {
		self.expect_char(Dump, '/');
		self.expect_char(Dump, '/');
		self.consume_while(Dump,
			|c| c != '\n' && c != '\0');
		self.consume(Dump);
		self.make(Token::Comment)
	}

	fn scan_multi_line_comment(&mut self) -> TokenAndSpan {
		use token::Token::{Comment};
		self.expect_char(Dump, '/');
		self.expect_char(Dump, '*');
		'outer: loop {
			match self.peek_2() {
				('*', '/') => {
					self.consume_n(Dump, 2);
					break 'outer;
				},
				('\0', _ ) => {
					// Error: unterminated multi line comment
					break 'outer;
				},
				_ => self.consume(Dump)
			};
		}
		self.make(Comment)
	}

	fn scan_identifier(&mut self) -> TokenAndSpan {
		self.expect_by(Keep, |c| c.is_alphabetic());
		self.consume_while(Keep, |c| c.is_alphanumeric() || c == '_');
		self.make(Token::Identifier(self.fetch_name()))
	}

	fn scan_digits_accumulated(
		&mut self,
		real_radix: u32,
		scan_radix: u32,
		separator_config: SeparatorConfig
	)
		-> (usize, usize)
	{
		assert!(real_radix <= scan_radix);
		let mut count_digits = 0;
		let mut accumulated  = 0usize;
		loop {
			match self.peek() {
				c if c.is_digit(scan_radix) => {
					if let Some(digit) = c.to_digit(real_radix) {
						accumulated *= real_radix as usize;
						accumulated += digit as usize;
					}
					else {
						// Error: invalid digit for a base {real_radix} literal
					}
					if c.is_lowercase() {
						// Error: cannot have lowercase digit characters
					}
					count_digits += 1;
					self.consume(Keep);
				},
				'_' => {
					if separator_config == Separable {
						// Error: digit seperator is disabled in this context
					}
					self.consume(Keep);
				},
				 _  => return (count_digits, accumulated)
			}
		}
	}

	/// Scan through any digits (base `scan_radix`) or underscores,
	/// and return how many digits there were.
	///
	/// `real_radix` represents the true radix of the number we're
	/// interested in, and errors will be emitted for any digits
	/// between `real_radix` and `scan_radix`.
	fn scan_digits(&mut self, real_radix: u32, scan_radix: u32) -> usize {
		let (count_digits, _ ) = self.scan_digits_accumulated(
			real_radix, scan_radix, Separable);
		count_digits
	}

 	//==================================================================
 	// Number scanning routines
 	//==================================================================

	fn make_integer(&mut self) -> TokenAndSpan {
		use token::Token::*;
		use token::LiteralToken::Integer;
		self.make(Literal(Integer(self.fetch_name())))
	}

	fn scan_binary_literal(&mut self) -> TokenAndSpan {
		self.expect_char(Keep, 'b');
		let count_digits = self.scan_digits(2, 10);
		if count_digits == 0 {
			// Error: no valid digits found for octal literal
		}
		self.make_integer()
	}

	fn scan_octal_literal(&mut self) -> TokenAndSpan {
		self.expect_char(Keep, 'o');
		let count_digits = self.scan_digits(8, 10);
		if count_digits == 0 {
			// Error: no valid digits found for octal literal
		}
		self.make_integer()
	}

	fn scan_hexdec_literal(&mut self) -> TokenAndSpan {
		self.expect_char(Keep, 'x');
		let count_digits = self.scan_digits(16, 16);
		if count_digits == 0 {
			// Error: no valid digits found for hexdec literal
		}
		self.make_integer()
	}

	fn scan_decimal_literal(&mut self) -> TokenAndSpan {
		assert!(self.peek().is_digit(10) || self.peek() == '_');
		self.scan_digits(10, 10);
		match self.peek() {
			'.' => self.scan_possible_float_literal(),
			_ => self.make_integer()
		}
	}

	fn make_float(&mut self) -> TokenAndSpan {
		use token::Token::*;
		use token::LiteralToken::Float;
		self.make(Literal(Float(self.fetch_name())))
	}

	fn scan_float_literal_exponent(&mut self) -> TokenAndSpan {
		if self.peek() == 'e' {
			self.consume(Keep);
			let sign = self.peek();
			if sign == '+' || sign == '-' {
				self.consume(Keep);
			}
			else {
				// Error: expected sign ('+' / '-') after exponent sign 'e'
			}
			let (count_digits, _ ) =
				self.scan_digits_accumulated(10, 10, Contiguous);
			if count_digits == 0 {
				// Error: no valid digits found for exponent
			}
		}
		self.make_float()
	}

	fn scan_float_literal(&mut self) -> TokenAndSpan {
		self.expect_char(Keep, '.');
		self.expect_by(Keep, |c| c.is_digit(10));
		self.scan_digits(10, 10);
		self.scan_float_literal_exponent()
	}

	/// Be careful, a '.' following a sequence of digits doesn't
	/// nessecarily mean that the following characters are part of
	/// a float literal.
	/// E.g. a method call on an integer literal may be possible, too:
	///    42.foo()
	/// Or a range expression:
	///    0..10
	fn scan_possible_float_literal(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), '.');
		match self.peek_2() {
			('.',  c ) if c.is_digit(10)
				=> self.scan_float_literal(),
			_ 	=> self.make_integer()
		}
	}

	fn scan_number_literal(&mut self) -> TokenAndSpan {
		assert!(self.peek().is_digit(10));
		match self.peek() {
			'0' => match self.consume(Keep).peek() {
				'b' => self.scan_binary_literal(),
				'o' => self.scan_octal_literal(),
				'x' => self.scan_hexdec_literal(),
				'.' => self.scan_possible_float_literal(),

				/* decimal number literal */
				'0' ... '9' | '_' => self.scan_decimal_literal(),

				/* just a null (0) */
				_ => self.make_integer()
			},

			/* decimal number literal */
			_ => self.scan_decimal_literal(),
		}
	}

 	//==================================================================
 	// String scanning routines
 	//==================================================================

	fn scan_char_ascii_hexcode(&mut self) -> bool {
		self.expect_char(Keep, 'x');
		let (count_digits, accumulated) =
			self.scan_digits_accumulated(16, 16, Contiguous);
		if count_digits != 2 {
			// Error: requires exactly 2 digits within char literal
			return false;
		}
		if accumulated > 0x7F {
			// Error: this form of character escape may only be used within the range [\x00-\x7F]
			return false;
		}
		true
	}

	fn scan_char_unicode(&mut self, char_encoding: CharEncoding) -> bool {
		use std::char;
		use std::ascii::AsciiExt;
		self.expect_char(Keep, 'u');
		self.expect_char(Keep, '{');
		let (count_digits, accumulated) =
			self.scan_digits_accumulated(16, 16, Contiguous);
		let mut valid = true;
		if count_digits == 0 {
			// Error: empty unicode escape not supported
			valid = false;
		}
		if count_digits > 6 {
			// Error: overlong unicode escape (can have at most 6 digits)
			valid = false;
		}
		match char::from_u32(accumulated as u32) {
			Some(ch) => {
				if char_encoding == Ascii && !ch.is_ascii() {
					// Error: non ascii characters are not allowed in bytes or byte strings
					valid = false;
				}
			},
			None => {
				// Error: invalid unicode escape sequence
				valid = false
			}
		};
		self.expect_char(Keep, '}');
		valid
	}

    fn scan_char_or_byte_escape(
    	&mut self,
    	char_encoding: CharEncoding,
    	delim: char
    )
    	-> bool
    {
    	self.expect_char(Keep, '\\');
    	let peek_2 = self.peek_2();
		match self.peek() {
			'0'  | /* Null */
			'n'  | /* LineFeed */
			'r'  | /* CarryReturn */
			't'  | /* Tab */
			'\\'   /* BackSlash */ => {
				self.consume(Keep);
			},
			'x'  => return self.scan_char_ascii_hexcode(),
			'u'  => return self.scan_char_unicode(char_encoding),
			// },
			'\n' if delim == '"' => {
				self.consume_while(Keep, |c| c.is_whitespace());
			},
			'\r' if delim == '"' && peek_2 == ('\r','\n') => {
				self.consume_while(Keep, |c| c.is_whitespace());
			},
			'\'' if delim == '\'' => { /* Single-Quote */
				self.consume(Keep);
			},
			'"' if delim == '"' => { /* Double-Quote */
				self.consume(Keep);
			},
			_ => {
				// Error: unknown escape sequence!
				return false;
			}
		}
		true
    }

	fn scan_char_or_byte(
		&mut self,
		char_encoding: CharEncoding,
		delim: char
	)
		-> bool
	{
		use std::ascii::AsciiExt;
		let mut valid = true;
		match self.peek() {
			'\\' => {
				self.scan_char_or_byte_escape(char_encoding, delim);
			},
			'\0' => {
				// Error: invalid \0 character in char or byte sequence
				valid = false;
			},
			c => {
				if char_encoding == Ascii && !c.is_ascii() {
					// Error: only ascii allowed in byte string or characters
					valid = false;
				}
				self.consume(Keep);
			}
		}
		valid
	}

	fn scan_char_or_byte_delim(&mut self, char_encoding: CharEncoding) -> bool {
		self.expect_char(Dump, '\'');
		let valid = self.scan_char_or_byte(char_encoding, '\'');
		self.expect_char(Dump, '\'');
		valid
	}

	fn scan_char_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::Char;
		if !self.scan_char_or_byte_delim(Unicode) {
			// Error: invalid char literal
		}
		self.make(Literal(Char(self.fetch_name())))
	}

	fn scan_byte_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::Byte;
		self.expect_char(Dump, 'b');
		if !self.scan_char_or_byte_delim(Ascii) {
			// Error: invalid byte literal
		}
		self.make(Literal(Byte(self.fetch_name())))
	}

	fn scan_string(&mut self, char_encoding: CharEncoding) -> bool {
		self.expect_char(Dump, '"');
		let mut valid = true;
		while valid && self.peek() != '"' {
			valid &= self.scan_char_or_byte(char_encoding, '"');
		}
		self.expect_char(Dump, '"');
		valid
	}

	fn scan_string_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::String;
		let valid = self.scan_string(Unicode);
		if !valid {
			// Error: invalid string literal
		}
		self.make(Literal(String(self.fetch_name())))
	}

	fn scan_byte_string_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::ByteString;
		self.expect_char(Dump, 'b');
		let valid = self.scan_string(Ascii);
		if !valid {
			// Error: invalid byte string literal
		}
		self.make(Literal(ByteString(self.fetch_name())))
	}

	fn scan_raw_string(
		&mut self,
		char_encoding: CharEncoding
	)
		-> (bool, usize)
	{
		use std::ascii::AsciiExt;
		self.expect_char(Dump, 'r');
		let count_pounds = self.consume_counted(Dump, |c| c == '#');
		self.expect_char(Dump, '"');
		let mut valid = true;
		'outer: loop {
			match self.peek() {
				'"' => {
					let old_len = self.name_buffer.len();
					self.consume(Keep);
					for _ in 0..count_pounds {
						if self.peek() != '#' {
							continue 'outer;
						}
						self.consume(Keep);
					}
					self.name_buffer.truncate(old_len); // deletes delimiter ("#*)
					break;
				},
				c => {
					if c == '\0' {
						// Error: unterminated raw string literal
						valid = false;
						break;
					}
					if char_encoding == Ascii && !c.is_ascii() {
						// Error: raw byte string must be ASCII
						valid = false;
					}
				}
			}
			self.consume(Keep);
		}
		(valid, count_pounds)
	}

	fn scan_raw_string_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::RawString;
		let (valid, count_pounds) = self.scan_raw_string(Unicode);
		if !valid {
			// Error: invalid raw string literal
		}
		self.make(Literal(RawString(self.fetch_name(), count_pounds)))
	}

	fn scan_raw_byte_string_literal(&mut self) -> TokenAndSpan {
		use token::Token::Literal;
		use token::LiteralToken::RawByteString;
		self.expect_char(Dump, 'b');
		let (valid, count_pounds) = self.scan_raw_string(Ascii);
		if !valid {
			// Error: invalid raw byte string literal
		}
		self.make(Literal(RawByteString(self.fetch_name(), count_pounds)))
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum CharEncoding { Ascii, Unicode }
use self::CharEncoding::*;

// Types like the Lexer implement this trait as they are iterators
// over Tokens. Maybe it should be substituted simply with the Iterator<Token> trait.
// However, a next_token(...) method is more explicit than just next(...) and 
// it allows one to add new required methods (e.g. peek_token(...)) in future versions.
pub trait TokenStream {
    fn next_token(&mut self) -> TokenAndSpan;
}

impl TokenStream for Lexer {
	fn next_token(&mut self) -> TokenAndSpan {
		use token::Token::*;
		use token::DelimitToken::*;
		use token::BinOpToken::*;
		use token::RelOpToken::*;

		// self.clear_buffer();
		self.reset_span_consumed();
		self.reset_name_buffer();

		match self.peek() {
			/* Skip whitespace */
			c if c.is_whitespace() => {
				self.consume_while(Dump, |c| c.is_whitespace());
				self.make(Whitespace)
			},

			// /* Identifiers and keywords */
			c if c.is_alphabetic() => match self.peek_3() {
				('r', '"',  _ ) |
				('r', '#',  _ ) => self.scan_raw_string_literal(),
				('b', '\'', _ ) => self.scan_byte_literal(),
				('b', '"',  _ ) => self.scan_byte_string_literal(),
				('b', 'r', '"') |
				('b', 'r', '#') => self.scan_raw_byte_string_literal(),
				_               => self.scan_identifier()
			},

			/* Integer- and float literals and identifiers */
			c if c.is_digit(10) => self.scan_number_literal(),

			/* Opening delimiters */
			'(' => self.consume(Dump).make(OpenDelim(Paren)),
			'[' => self.consume(Dump).make(OpenDelim(Bracket)),
			'{' => self.consume(Dump).make(OpenDelim(Brace)),

			/* Opening delimiters */
			')' => self.consume(Dump).make(CloseDelim(Paren)),
			']' => self.consume(Dump).make(CloseDelim(Bracket)),
			'}' => self.consume(Dump).make(CloseDelim(Brace)),

			/* Special tokens which aren't the beginning
			   of any other token */
			'?' => self.consume(Dump).make(Question),
			';' => self.consume(Dump).make(SemiColon),
			',' => self.consume(Dump).make(Comma),
			'_' => self.consume(Dump).make(Underscore),

			/* Dot, DotDot and DotDotDot tokens */
			'.' => match self.consume(Dump).peek_2() {
				('.', '.') => self.consume_n(Dump, 2).make(DotDotDot),
				('.',  _ ) => self.consume(Dump).make(DotDot),
				( _ ,  _ ) => self.make(Dot)
			},

			/* Tokens starting with ':' */
			':' => match self.consume(Dump).peek() {
				':' => self.consume(Dump).make(ColonColon),
				_   => self.make(Colon)
			},

			/* Tokens starting with '+' */
			'+' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Plus)),
				_   => self.make(BinOp(Plus))
			},

			/* Tokens starting with '-' */
			'-' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Minus)),
				'>' => self.consume(Dump).make(Arrow),
				_   => self.make(BinOp(Minus))
			},

			/* Tokens starting with '*' */
			'*' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Star)),
				_   => self.make(BinOp(Star))
			},

			/* Tokens starting with '/' */
			'/' => match self.peek_2() {
				( _ , '=') => self.consume_n(Dump, 2).make(BinOpEq(Slash)),
				( _ , '/') => self.scan_line_comment(),
				( _ , '*') => self.scan_multi_line_comment(),
				( _ ,  _ ) => self.consume(Dump).make(BinOp(Slash))
			},

			/* Tokens starting with '%' */
			'%' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Percent)),
				_   => self.make(BinOp(Percent))
			},

			/* Tokens starting with '^' */
			'^' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Caret)),
				_   => self.make(BinOp(Caret))
			},

			/* Tokens starting with '!' */
			'!' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(RelOp(NotEq)),
				_   => self.make(Exclamation)
			},

			/* Tokens starting with '=' */
			'=' => match self.consume(Dump).peek() {
				'>' => self.consume(Dump).make(FatArrow),
				'=' => self.consume(Dump).make(RelOp(EqEq)),
				_   => self.make(Eq)
			},

			/* Tokens starting with '&' */
			'&' => match self.consume(Dump).peek() {
				'&' => self.consume(Dump).make(BinOp(AmpAmp)),
				'=' => self.consume(Dump).make(BinOpEq(And)),
				_   => self.make(BinOp(And))
			},

			/* Tokens starting with '|' */
			'|' => match self.consume(Dump).peek() {
				'|' => self.consume(Dump).make(BinOp(PipePipe)),
				'=' => self.consume(Dump).make(BinOpEq(Or)),
				_   => self.make(BinOp(Or))
			},

			/* Tokens starting with '<' */
			'<' => match self.consume(Dump).peek_2() {
				('<', '=') => self.consume_n(Dump, 2).make(BinOpEq(Shl)),
				('<',  _ ) => self.consume(Dump).make(BinOp(Shl)),
				('=',  _ ) => self.consume(Dump).make(RelOp(LessEq)),
				( _ ,  _ ) => self.make(RelOp(LessThan))
			},

			/* Tokens starting with '>' */
			'>' => match self.consume(Dump).peek_2() {
				('>', '=') => self.consume_n(Dump, 2).make(BinOpEq(Shr)),
				('>',  _ ) => self.consume(Dump).make(BinOp(Shr)),
				('=',  _ ) => self.consume(Dump).make(RelOp(GreaterEq)),
				( _ ,  _ ) => self.make(RelOp(GreaterThan))
			},

			/* Char and string literals */
			'\'' => self.scan_char_literal(),
			'"' => self.scan_string_literal(),

			/* When end of iterator has been reached */
			_ => self.make(EndOfFile)
		}
	}
}

impl Iterator for Lexer {
	type Item = TokenAndSpan;

	fn next(&mut self) -> Option<Self::Item> {
		let tok_span = self.next_token();
		match tok_span {
			TokenAndSpan { tok: Token::EndOfFile, sp: _ } => None,
			_                                             => Some(tok_span)
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use ::token::Token;
	use parse_sess::{ParseSess};
	use code_map::Span;

	fn check_lexer_output_against(
		lexer: &mut Lexer,
		check_against: &[(Token, (usize, usize))]
	) {
		for &(tok, (lo, hi)) in check_against {
			let tas = TokenAndSpan {
				tok: tok,
				sp: Span::from_usize(lo, hi)
			};
			let next = lexer.next();
			// println!("(next, tas) = ({:?}, {:?})", &next, &tas);
			assert_eq!(/*lexer.next()*/next, Some(tas));
		}
		assert_eq!(lexer.next(), None);
	}

	#[test]
	fn single_byte_tokens() {
		use token::Token::*;
		use token::DelimitToken::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap("fm1", "()[]{}?;,_");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(OpenDelim(Paren),    (0, 0)),
			(CloseDelim(Paren),   (1, 1)),
			(OpenDelim(Bracket),  (2, 2)),
			(CloseDelim(Bracket), (3, 3)),
			(OpenDelim(Brace),    (4, 4)),
			(CloseDelim(Brace),   (5, 5)),
			(Question,            (6, 6)),
			(SemiColon,           (7, 7)),
			(Comma,               (8, 8)),
			(Underscore,          (9, 9))
		]);
	}

	#[test]
	fn comments() {
		use token::Token::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"//foo\n  /*bar\nbaz\n*/\n /*** //foo//***/");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Comment,    ( 0,  5)),
			(Whitespace, ( 6,  7)),
			(Comment,    ( 8, 19)),
			(Whitespace, (20, 21)),
			(Comment,    (22, 37)),
		]);
	}

	#[test]
	fn finite_tokens() {
		use token::Token::*;
		use token::BinOpToken::*;
		use token::RelOpToken::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			". .. ...  \
			 + +=      \
			 - -= ->   \
			 * *=      \
			 / /=      \
			 % %=      \
			 ^ ^=      \
			 ! !=      \
			 = => ==   \
			 & && &=   \
			 | || |=   \
			 : ::      \
			 < << <<= <= \
			 > >> >>= >="); // <= 10 columns per row!
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Dot,            ( 0,  0)),
			(Whitespace,     ( 1,  1)),
			(DotDot,         ( 2,  3)),
			(Whitespace,     ( 4,  4)),
			(DotDotDot,      ( 5,  7)),
			(Whitespace,     ( 8,  9)),

			(BinOp(Plus),    (10, 10)),
			(Whitespace,     (11, 11)),
			(BinOpEq(Plus),  (12, 13)),
			(Whitespace,     (14, 19)),

			(BinOp(Minus),   (20, 20)),
			(Whitespace,     (21, 21)),
			(BinOpEq(Minus), (22, 23)),
			(Whitespace,     (24, 24)),
			(Arrow,          (25, 26)),
			(Whitespace,     (27, 29)),

			(BinOp(Star),    (30, 30)),
			(Whitespace,     (31, 31)),
			(BinOpEq(Star),  (32, 33)),
			(Whitespace,     (34, 39)),

			(BinOp(Slash),   (40, 40)),
			(Whitespace,     (41, 41)),
			(BinOpEq(Slash), (42, 43)),
			(Whitespace,     (44, 49)),

			(BinOp(Percent), (50, 50)),
			(Whitespace,     (51, 51)),
			(BinOpEq(Percent), (52, 53)),
			(Whitespace,     (54, 59)),

			(BinOp(Caret),   (60, 60)),
			(Whitespace,     (61, 61)),
			(BinOpEq(Caret), (62, 63)),
			(Whitespace,     (64, 69)),

			(Exclamation,    (70, 70)),
			(Whitespace,     (71, 71)),
			(RelOp(NotEq),   (72, 73)),
			(Whitespace,     (74, 79)),

			(Eq,             (80, 80)),
			(Whitespace,     (81, 81)),
			(FatArrow,       (82, 83)),
			(Whitespace,     (84, 84)),
			(RelOp(EqEq),    (85, 86)),
			(Whitespace,     (87, 89)),

			(BinOp(And),     (90, 90)),
			(Whitespace,     (91, 91)),
			(BinOp(AmpAmp),  (92, 93)),
			(Whitespace,     (94, 94)),
			(BinOpEq(And),   (95, 96)),
			(Whitespace,     (97, 99)),

			(BinOp(Or),      (100, 100)),
			(Whitespace,     (101, 101)),
			(BinOp(PipePipe),(102, 103)),
			(Whitespace,     (104, 104)),
			(BinOpEq(Or),    (105, 106)),
			(Whitespace,     (107, 109)),

			(Colon,          (110, 110)),
			(Whitespace,     (111, 111)),
			(ColonColon,     (112, 113)),
			(Whitespace,     (114, 119)),

			(RelOp(LessThan),(120, 120)),
			(Whitespace,     (121, 121)),
			(BinOp(Shl),     (122, 123)),
			(Whitespace,     (124, 124)),
			(BinOpEq(Shl),   (125, 127)),
			(Whitespace,     (128, 128)),
			(RelOp(LessEq),  (129, 130)),
			(Whitespace,     (131, 131)),

			(RelOp(GreaterThan),(132, 132)),
			(Whitespace,     (133, 133)),
			(BinOp(Shr),     (134, 135)),
			(Whitespace,     (136, 136)),
			(BinOpEq(Shr),   (137, 139)),
			(Whitespace,     (140, 140)),
			(RelOp(GreaterEq), (141, 142)),
		]);
	}

	#[test]
	fn whitespace() {
		use token::Token::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"   .\n\n\n.\r\n\r\n.\t\t\t");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace, ( 0,  2)),
			(Dot,        ( 3,  3)),
			(Whitespace, ( 4,  6)),
			(Dot,        ( 7,  7)),
			(Whitespace, ( 8, 11)),
			(Dot,        (12, 12)),
			(Whitespace, (13, 15)),
		]);
	}

	#[test]
	fn simple_identifiers() {
		use token::Token::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"true false \
			 alphanumeric \
			 with_underscore \
			 BiGaNdSmAlL \
			 underscores_at_the_end__ \
			 with_N0m3r5 \
			 j__u___5__T");
		let names = ctx.symbol_table.borrow_mut().prefill(&[
			"true",
			"false",
			"alphanumeric",
			"with_underscore",
			"BiGaNdSmAlL",
			"underscores_at_the_end__",
			"with_N0m3r5",
			"j__u___5__T"
		]);
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Identifier(names[0]), ( 0,  3)),
			(Whitespace,           ( 4,  4)),
			(Identifier(names[1]), ( 5,  9)),
			(Whitespace,           (10, 10)),
			(Identifier(names[2]), (11, 22)),
			(Whitespace,           (23, 23)),
			(Identifier(names[3]), (24, 38)),
			(Whitespace,           (39, 39)),
			(Identifier(names[4]), (40, 50)),
			(Whitespace,           (51, 51)),
			(Identifier(names[5]), (52, 75)),
			(Whitespace,           (76, 76)),
			(Identifier(names[6]), (77, 87)),
			(Whitespace,           (88, 88)),
			(Identifier(names[7]), (89, 99))
		]);
	}

	#[test]
	fn char_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Char;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r#" 'a' 'Z' '"' ' ' '\t' '\r' '\n' ':' '\0' '\\' '\'' "#);
		let name_a         = ctx.symbol_table.borrow_mut().intern("a");
		let name_z         = ctx.symbol_table.borrow_mut().intern("Z");
		let name_dbl_quote = ctx.symbol_table.borrow_mut().intern("\"");
		let name_space     = ctx.symbol_table.borrow_mut().intern(" ");
		let name_tab       = ctx.symbol_table.borrow_mut().intern(r"\t");
		let name_carry_ret = ctx.symbol_table.borrow_mut().intern(r"\r");
		let name_linefeed  = ctx.symbol_table.borrow_mut().intern(r"\n");
		let name_colon     = ctx.symbol_table.borrow_mut().intern(":");
		let name_null      = ctx.symbol_table.borrow_mut().intern(r"\0");
		let name_backslash = ctx.symbol_table.borrow_mut().intern(r"\\");
		let name_quote     = ctx.symbol_table.borrow_mut().intern(r"\'");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                    ( 0,  0)),
			(Literal(Char(name_a)),         ( 1,  3)),
			(Whitespace,                    ( 4,  4)),
			(Literal(Char(name_z)),         ( 5,  7)),
			(Whitespace,                    ( 8,  8)),
			(Literal(Char(name_dbl_quote)), ( 9, 11)),
			(Whitespace,                    (12, 12)),
			(Literal(Char(name_space)),     (13, 15)),
			(Whitespace,                    (16, 16)),
			(Literal(Char(name_tab)),       (17, 20)),
			(Whitespace,                    (21, 21)),
			(Literal(Char(name_carry_ret)), (22, 25)),
			(Whitespace,                    (26, 26)),
			(Literal(Char(name_linefeed)),  (27, 30)),
			(Whitespace,                    (31, 31)),
			(Literal(Char(name_colon)),     (32, 34)),
			(Whitespace,                    (35, 35)),
			(Literal(Char(name_null)),      (36, 39)),
			(Whitespace,                    (40, 40)),
			(Literal(Char(name_backslash)), (41, 44)),
			(Whitespace,                    (45, 45)),
			(Literal(Char(name_quote)),     (46, 49)),
			(Whitespace,                    (50, 50))
		]);
	}

	#[test]
	fn byte_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Byte;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r#" b'a' b'Z' b'"' b' ' b'\t' b'\r' b'\n' b':' b'\0' b'\\' b'\'' "#);
		let name_a         = ctx.symbol_table.borrow_mut().intern("a");
		let name_z         = ctx.symbol_table.borrow_mut().intern("Z");
		let name_dbl_quote = ctx.symbol_table.borrow_mut().intern("\"");
		let name_space     = ctx.symbol_table.borrow_mut().intern(" ");
		let name_tab       = ctx.symbol_table.borrow_mut().intern(r"\t");
		let name_carry_ret = ctx.symbol_table.borrow_mut().intern(r"\r");
		let name_linefeed  = ctx.symbol_table.borrow_mut().intern(r"\n");
		let name_colon     = ctx.symbol_table.borrow_mut().intern(":");
		let name_null      = ctx.symbol_table.borrow_mut().intern(r"\0");
		let name_backslash = ctx.symbol_table.borrow_mut().intern(r"\\");
		let name_quote     = ctx.symbol_table.borrow_mut().intern(r"\'");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                    ( 0,  0)),
			(Literal(Byte(name_a)),         ( 1,  4)),
			(Whitespace,                    ( 5,  5)),
			(Literal(Byte(name_z)),         ( 6,  9)),
			(Whitespace,                    (10, 10)),
			(Literal(Byte(name_dbl_quote)), (11, 14)),
			(Whitespace,                    (15, 15)),
			(Literal(Byte(name_space)),     (16, 19)),
			(Whitespace,                    (20, 20)),
			(Literal(Byte(name_tab)),       (21, 25)),
			(Whitespace,                    (26, 26)),
			(Literal(Byte(name_carry_ret)), (27, 31)),
			(Whitespace,                    (32, 32)),
			(Literal(Byte(name_linefeed)),  (33, 37)),
			(Whitespace,                    (38, 38)),
			(Literal(Byte(name_colon)),     (39, 42)),
			(Whitespace,                    (43, 43)),
			(Literal(Byte(name_null)),      (44, 48)),
			(Whitespace,                    (49, 49)),
			(Literal(Byte(name_backslash)), (50, 54)),
			(Whitespace,                    (55, 55)),
			(Literal(Byte(name_quote)),     (56, 60)),
			(Whitespace,                    (61, 61))
		]);
	}

	#[test]
	fn char_ascii_escape_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Char;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r" '\x00' '\x7F' '\x09' ");

		let name_00 = ctx.symbol_table.borrow_mut().intern(r"\x00");

		#[allow(non_snake_case)]
		let name_7F = ctx.symbol_table.borrow_mut().intern(r"\x7F");

		#[allow(non_snake_case)]
		let name_09 = ctx.symbol_table.borrow_mut().intern(r"\x09");

		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,             ( 0,  0)),
			(Literal(Char(name_00)), ( 1,  6)),
			(Whitespace,             ( 7,  7)),
			(Literal(Char(name_7F)), ( 8, 13)),
			(Whitespace,             (14, 14)),
			(Literal(Char(name_09)), (15, 20)),
			(Whitespace,             (21, 21)),
		]);
	}

	#[test]
	fn byte_ascii_escape_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Byte;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r" b'\x00' b'\x7F' b'\x09' ");
		let name_00 = ctx.symbol_table.borrow_mut().intern(r"\x00");
		#[allow(non_snake_case)]
		let name_7F = ctx.symbol_table.borrow_mut().intern(r"\x7F");
		let name_09 = ctx.symbol_table.borrow_mut().intern(r"\x09");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,             ( 0,  0)),
			(Literal(Byte(name_00)), ( 1,  7)),
			(Whitespace,             ( 8,  8)),
			(Literal(Byte(name_7F)), ( 9, 15)),
			(Whitespace,             (16, 16)),
			(Literal(Byte(name_09)), (17, 23)),
			(Whitespace,             (24, 24)),
		]);
	}

	#[test]
	fn char_unicode_escape_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Char;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r" '\u{0}' '\u{1337}' '\u{0FFFFF}' '\u{100000}' ");
		let name_0      = ctx.symbol_table.borrow_mut().intern(r"\u{0}");
		let name_1337   = ctx.symbol_table.borrow_mut().intern(r"\u{1337}");
		#[allow(non_snake_case)]
		let name_0FFFFF = ctx.symbol_table.borrow_mut().intern(r"\u{0FFFFF}");
		let name_100000 = ctx.symbol_table.borrow_mut().intern(r"\u{100000}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                 ( 0,  0)),
			(Literal(Char(name_0     )), ( 1,  7)),
			(Whitespace,                 ( 8,  8)),
			(Literal(Char(name_1337  )), ( 9, 18)),
			(Whitespace,                 (19, 19)),
			(Literal(Char(name_0FFFFF)), (20, 31)),
			(Whitespace,                 (32, 32)),
			(Literal(Char(name_100000)), (33, 44)),
			(Whitespace,                 (45, 45)),
		]);
	}

	#[test]
	fn byte_unicode_escape_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Byte;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r" b'\u{0}' b'\u{000001}' b'\u{7F}' b'\u{42}' ");
		let name_0 = ctx.symbol_table.borrow_mut().intern(r"\u{0}");
		let name_1 = ctx.symbol_table.borrow_mut().intern(r"\u{000001}");
		let name_2 = ctx.symbol_table.borrow_mut().intern(r"\u{7F}");
		let name_3 = ctx.symbol_table.borrow_mut().intern(r"\u{42}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,            ( 0,  0)),
			(Literal(Byte(name_0)), ( 1,  8)),
			(Whitespace,            ( 9,  9)),
			(Literal(Byte(name_1)), (10, 22)),
			(Whitespace,            (23, 23)),
			(Literal(Byte(name_2)), (24, 32)),
			(Whitespace,            (33, 33)),
			(Literal(Byte(name_3)), (34, 42)),
			(Whitespace,            (43, 43)),
		]);
	}

	#[test]
	fn decimal_integer_literals() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Integer;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"0 42 1337 1_234_567_890 007 1__");
		let name_0             = ctx.symbol_table.borrow_mut().intern(r"0");
		let name_42            = ctx.symbol_table.borrow_mut().intern(r"42");
		let name_1337          = ctx.symbol_table.borrow_mut().intern(r"1337");
		let name_1_234_567_890 = ctx.symbol_table.borrow_mut().intern(r"1_234_567_890");
		let name_007           = ctx.symbol_table.borrow_mut().intern(r"007");
		let name_1__           = ctx.symbol_table.borrow_mut().intern(r"1__");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Integer(name_0)),    ( 0,  0)),
			(Whitespace,                  ( 1,  1)),
			(Literal(Integer(name_42)),   ( 2,  3)),
			(Whitespace,                  ( 4,  4)),
			(Literal(Integer(name_1337)), ( 5,  8)),
			(Whitespace,                  ( 9,  9)),
			(Literal(Integer(name_1_234_567_890)), (10, 22)),
			(Whitespace,                  (23, 23)),
			(Literal(Integer(name_007)),  (24, 26)),
			(Whitespace,                  (27, 27)),
			(Literal(Integer(name_1__)),  (28, 30)),
		]);
	}

	#[test]
	fn binary_integer_literals() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Integer;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"0b0 0b1 0b0__ 0b__1 0b11____11 0b_0000_0101_1111");
		let name_0               = ctx.symbol_table.borrow_mut().intern(r"0b0");
		let name_1               = ctx.symbol_table.borrow_mut().intern(r"0b1");
		let name_0__             = ctx.symbol_table.borrow_mut().intern(r"0b0__");
		let name___1             = ctx.symbol_table.borrow_mut().intern(r"0b__1");
		let name_11__11          = ctx.symbol_table.borrow_mut().intern(r"0b11____11");
		let name__0000_0101_1111 = ctx.symbol_table.borrow_mut().intern(r"0b_0000_0101_1111");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Integer(name_0)),      ( 0,  2)),
			(Whitespace,                    ( 3,  3)),
			(Literal(Integer(name_1)),      ( 4,  6)),
			(Whitespace,                    ( 7,  7)),
			(Literal(Integer(name_0__)),    ( 8, 12)),
			(Whitespace,                    (13, 13)),
			(Literal(Integer(name___1)),    (14, 18)),
			(Whitespace,                    (19, 19)),
			(Literal(Integer(name_11__11)), (20, 29)),
			(Whitespace,                    (30, 30)),
			(Literal(Integer(name__0000_0101_1111)), (31, 47)),
		]);
	}

	#[test]
	fn octal_integer_literals() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Integer;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"0o0 0o1 0o0__ 0o__7 0o42____51 0o_123_456_777");
		let name_0            = ctx.symbol_table.borrow_mut().intern(r"0o0");
		let name_1            = ctx.symbol_table.borrow_mut().intern(r"0o1");
		let name_0__          = ctx.symbol_table.borrow_mut().intern(r"0o0__");
		let name___7          = ctx.symbol_table.borrow_mut().intern(r"0o__7");
		let name_42__52       = ctx.symbol_table.borrow_mut().intern(r"0o42____51");
		let name__123_456_777 = ctx.symbol_table.borrow_mut().intern(r"0o_123_456_777");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Integer(name_0)),      ( 0,  2)),
			(Whitespace,                    ( 3,  3)),
			(Literal(Integer(name_1)),      ( 4,  6)),
			(Whitespace,                    ( 7,  7)),
			(Literal(Integer(name_0__)),    ( 8, 12)),
			(Whitespace,                    (13, 13)),
			(Literal(Integer(name___7)),    (14, 18)),
			(Whitespace,                    (19, 19)),
			(Literal(Integer(name_42__52)), (20, 29)),
			(Whitespace,                    (30, 30)),
			(Literal(Integer(name__123_456_777)), (31, 44)),
		]);
	}

	#[test]
	fn hexdec_integer_literals() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Integer;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"0x0 0xF 0x0__ 0x__A 0xA9____B2 0x_0123_4567_89AB_CDEF");
		let name_0 = ctx.symbol_table.borrow_mut().intern(r"0x0");
		let name_1 = ctx.symbol_table.borrow_mut().intern(r"0xF");
		let name_2 = ctx.symbol_table.borrow_mut().intern(r"0x0__");
		let name_3 = ctx.symbol_table.borrow_mut().intern(r"0x__A");
		let name_4 = ctx.symbol_table.borrow_mut().intern(r"0xA9____B2");
		let name_5 = ctx.symbol_table.borrow_mut().intern(r"0x_0123_4567_89AB_CDEF");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Integer(name_0)), ( 0,  2)),
			(Whitespace,               ( 3,  3)),
			(Literal(Integer(name_1)), ( 4,  6)),
			(Whitespace,               ( 7,  7)),
			(Literal(Integer(name_2)), ( 8, 12)),
			(Whitespace,               (13, 13)),
			(Literal(Integer(name_3)), (14, 18)),
			(Whitespace,               (19, 19)),
			(Literal(Integer(name_4)), (20, 29)),
			(Whitespace,               (30, 30)),
			(Literal(Integer(name_5)), (31, 52)),
		]);
	}

	#[test]
	fn float_literals() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::Float;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"0.0       \
			 42.0      \
			 0.24      \
			 13.37     \
			 0.00_00_1 \
			 1.23e+12  \
			 0.01e-07  \
			 1_.1_     ");
		let name_0 = ctx.symbol_table.borrow_mut().intern(r"0.0");
		let name_1 = ctx.symbol_table.borrow_mut().intern(r"42.0");
		let name_2 = ctx.symbol_table.borrow_mut().intern(r"0.24");
		let name_3 = ctx.symbol_table.borrow_mut().intern(r"13.37");
		let name_4 = ctx.symbol_table.borrow_mut().intern(r"0.00_00_1");
		let name_5 = ctx.symbol_table.borrow_mut().intern(r"1.23e+12");
		let name_6 = ctx.symbol_table.borrow_mut().intern(r"0.01e-07");
		let name_7 = ctx.symbol_table.borrow_mut().intern(r"1_.1_");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Float(name_0)), ( 0,  2)),
			(Whitespace,             ( 3,  9)),
			(Literal(Float(name_1)), (10, 13)),
			(Whitespace,             (14, 19)),
			(Literal(Float(name_2)), (20, 23)),
			(Whitespace,             (24, 29)),
			(Literal(Float(name_3)), (30, 34)),
			(Whitespace,             (35, 39)),
			(Literal(Float(name_4)), (40, 48)),
			(Whitespace,             (49, 49)),
			(Literal(Float(name_5)), (50, 57)),
			(Whitespace,             (58, 59)),
			(Literal(Float(name_6)), (60, 67)),
			(Whitespace,             (68, 69)),
			(Literal(Float(name_7)), (70, 74)),
			(Whitespace,             (75, 79)),
		]);
	}

	#[test]
	fn dot_after_number_sequence() {
		use token::Token::{BinOp, OpenDelim, CloseDelim, Identifier, Literal, Dot, DotDot, Whitespace};
		use token::LiteralToken::{Integer, Float};
		use token::BinOpToken::Minus;
		use token::DelimitToken::*;
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"17.foo() 0xABC.exp() 0b110..0o736 0..9 1.23..45.6 5.e-12");
		let names = ctx.symbol_table.borrow_mut().prefill(&[
			r"17",
			r"foo",
			r"0xABC",
			r"exp",
			r"0b110",
			r"0o736",
			r"0",
			r"9",
			r"1.23",
			r"45.6",
			r"5",
			r"e",
			r"12",
		]);
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Literal(Integer(names[0])),  ( 0,  1)),
			(Dot,                       ( 2,  2)),
			(Identifier(names[1]),        ( 3,  5)),
			(OpenDelim(Paren),          ( 6,  6)),
			(CloseDelim(Paren),         ( 7,  7)),
			(Whitespace,                ( 8,  8)),
			(Literal(Integer(names[2])),  ( 9, 13)),
			(Dot,                       (14, 14)),
			(Identifier(names[3]),        (15, 17)),
			(OpenDelim(Paren),          (18, 18)),
			(CloseDelim(Paren),         (19, 19)),
			(Whitespace,                (20, 20)),
			(Literal(Integer(names[4])),  (21, 25)),
			(DotDot,                    (26, 27)),
			(Literal(Integer(names[5])),  (28, 32)),
			(Whitespace,                (33, 33)),
			(Literal(Integer(names[6])),  (34, 34)),
			(DotDot,                    (35, 36)),
			(Literal(Integer(names[7])),  (37, 37)),
			(Whitespace,                (38, 38)),
			(Literal(Float(names[8])),    (39, 42)),
			(DotDot,                    (43, 44)),
			(Literal(Float(names[9])),    (45, 48)),
			(Whitespace,                (49, 49)),
			(Literal(Integer(names[10])), (50, 50)),
			(Dot,                       (51, 51)),
			(Identifier(names[11]),       (52, 52)),
			(BinOp(Minus),              (53, 53)),
			(Literal(Integer(names[12])), (54, 55)),
		]);
	}

	#[test]
	fn string_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::{String};
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r###" "Hello, World!" "\"" "'" "\n\t\x7F\u{1337}" "###);
		let name_0  = ctx.symbol_table.borrow_mut().intern(r"Hello, World!");
		let name_1  = ctx.symbol_table.borrow_mut().intern(r#"\""#);
		let name_2  = ctx.symbol_table.borrow_mut().intern(r"'");
		let name_3  = ctx.symbol_table.borrow_mut().intern(r"\n\t\x7F\u{1337}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,              ( 0,  0)),
			(Literal(String(name_0)), ( 1, 15)),
			(Whitespace,              (16, 16)),
			(Literal(String(name_1)), (17, 20)),
			(Whitespace,              (21, 21)),
			(Literal(String(name_2)), (22, 24)),
			(Whitespace,              (25, 25)),
			(Literal(String(name_3)), (26, 43)),
			(Whitespace,              (44, 44)),
		]);
	}

	#[test]
	fn string_literal_escape_whitespace() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::{String};
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r#" "Hello, \
			     World!""#);
		let name_0 = ctx.symbol_table.borrow_mut().intern(
			"Hello, \\\n\t\t\t     World!");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,              ( 0,  0)),
			(Literal(String(name_0)), ( 1, 25)),
		]);
	}

	#[test]
	fn byte_string_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::{ByteString};
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r###" b"Hello, World!" b"\"" b"'" b"\n\t\x7F\u{42}" "###);
		let name_0  = ctx.symbol_table.borrow_mut().intern(r"Hello, World!");
		let name_1  = ctx.symbol_table.borrow_mut().intern(r#"\""#);
		let name_2  = ctx.symbol_table.borrow_mut().intern(r"'");
		let name_3  = ctx.symbol_table.borrow_mut().intern(r"\n\t\x7F\u{42}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                  ( 0,  0)),
			(Literal(ByteString(name_0)), ( 1, 16)),
			(Whitespace,                  (17, 17)),
			(Literal(ByteString(name_1)), (18, 22)),
			(Whitespace,                  (23, 23)),
			(Literal(ByteString(name_2)), (24, 27)),
			(Whitespace,                  (28, 28)),
			(Literal(ByteString(name_3)), (29, 45)),
			(Whitespace,                  (46, 46)),
		]);
	}

	#[test]
	fn raw_string_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::{RawString};
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r###" r"Hello, World!" r##"\""## r#"'"# r"\n\t\x7F\u{0F0F0}" "###);
		let name_0  = ctx.symbol_table.borrow_mut().intern(r"Hello, World!");
		let name_1  = ctx.symbol_table.borrow_mut().intern(r#"\""#);
		let name_2  = ctx.symbol_table.borrow_mut().intern(r"'");
		let name_3  = ctx.symbol_table.borrow_mut().intern(r"\n\t\x7F\u{0F0F0}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                    ( 0,  0)),
			(Literal(RawString(name_0, 0)), ( 1, 16)),
			(Whitespace,                    (17, 17)),
			(Literal(RawString(name_1, 2)), (18, 26)),
			(Whitespace,                    (27, 27)),
			(Literal(RawString(name_2, 1)), (28, 33)),
			(Whitespace,                    (34, 34)),
			(Literal(RawString(name_3, 0)), (35, 54)),
			(Whitespace,                    (55, 55)),
		]);
	}

	#[test]
	fn raw_byte_string_literal() {
		use token::Token::{Literal, Whitespace};
		use token::LiteralToken::{RawByteString};
		let ctx = ParseSess::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			r###" br"Hello, World!" br##"\""## br#"'"# br"\n\t\x7F\u{0F0F0F}" "###);
		let name_0 = ctx.symbol_table.borrow_mut().intern(r"Hello, World!");
		let name_1 = ctx.symbol_table.borrow_mut().intern(r#"\""#);
		let name_2 = ctx.symbol_table.borrow_mut().intern(r"'");
		let name_3 = ctx.symbol_table.borrow_mut().intern(r"\n\t\x7F\u{0F0F0F}");
		let mut lexer = Lexer::new_for_filemap(ctx, &fm);
		check_lexer_output_against(&mut lexer, &[
			(Whitespace,                        ( 0,  0)),
			(Literal(RawByteString(name_0, 0)), ( 1, 17)),
			(Whitespace,                        (18, 18)),
			(Literal(RawByteString(name_1, 2)), (19, 28)),
			(Whitespace,                        (29, 29)),
			(Literal(RawByteString(name_2, 1)), (30, 36)),
			(Whitespace,                        (37, 37)),
			(Literal(RawByteString(name_3, 0)), (38, 59)),
			(Whitespace,                        (60, 60)),
		]);
	}
}
