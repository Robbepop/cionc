use std::collections::VecDeque;

use util::is_any_of::*;
use parser::util::char_util::CharProperties;
use parser::token::*;
use parser::compile_context::CompileContext;
use parser::string_cache::Name;
use parser::code_map::{FileMap, FileMapIterator, CharAndPos, Span};

// This is the lexer implementation for the parser (that sadly doesn't exist yet).
//
// This implementation is still experimental!
// Many things like scan_string_literal(...) are still missing or are not implemented at all.

#[derive(PartialEq, Eq, Debug)]
pub struct TokenAndSpan {
	tok: Token,
	sp: Span
}

pub struct Lexer<'ctx> {
	context: &'ctx CompileContext,
	input: FileMapIterator,
	peeked: VecDeque<CharAndPos>,
	name_buffer: String,
	span_consumed: Span
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ConsumeBehaviour {
	Dump,
	Keep
}
use self::ConsumeBehaviour::*;

impl<'ctx> Lexer<'ctx> {
	pub fn new_for_filemap(
		ctx: &'ctx CompileContext,
		fm: &FileMap
	)
		-> Lexer<'ctx>
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
		assert_eq!(self.peeked.is_empty(), false);
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

	fn consume_counted<C, U>(
		&mut self,
		behaviour: ConsumeBehaviour,
		pred_counted: C,
		pred_uncounted: U
	) -> usize
		where C: Fn(char) -> bool,
		      U: Fn(char) -> bool
	{
		let mut count_valid = 0;
		loop {
			let peeded = self.peek();
			if pred_counted(peeded) {
				count_valid += 1;
				self.consume(behaviour);
			}
			else if pred_uncounted(peeded) {
				self.consume(behaviour);
			}
			else {
				break;
			}
		}
		count_valid
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

	fn expect_char(&mut self, behaviour: ConsumeBehaviour, ch: char) -> &mut Self {
		self.consume(behaviour);
		if self.peek() == ch {
			self.consume(behaviour);
		}
		else {
			// TODO: error! expected 'ch' character!
		}
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
		self.context.string_cache.borrow_mut().intern(&self.name_buffer)
	}

	fn scan_line_comment(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), '/');
		self.consume_while(Dump, |c| c.is_none_of(&['\n','\0']));
		self.consume(Dump);
		self.make(Token::Comment)
	}

	fn scan_multi_line_comment(&mut self) -> TokenAndSpan {
		use parser::token::Token::{Comment, Error};
		assert_eq!(self.peek(), '*');
		self.consume(Dump);
		loop {
			match self.peek_2() {
				('*', '/') => return self.consume_n(Dump, 2).make(Comment),
				('\0', _ ) => return self.make(Error),
				_ => self.consume(Dump)
			};
		}
	}

	fn scan_identifier(&mut self) -> TokenAndSpan {
		assert!(self.peek().is_alpha());
		self.consume_while(Keep, |c| c.is_alpha_numeral() || c == '_');
		self.make(Token::Identifier(self.fetch_name()))
	}

	fn scan_char_suffix(&mut self) -> TokenAndSpan {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Char;
		assert_eq!(self.peek(), '\'');
		match self.consume(Dump).peek() {
			c if c.is_alpha() => {
				self.consume_while(Keep, |c| c.is_alpha_numeral() || c == '_');
				self.make(Literal(Char(self.fetch_name())))
			},
			_ => self.make(Literal(Char(self.fetch_name())))
		}
	}

	// Accepts closed char sequences and forwards to the suffix scanning routine.
	// e.g. 'a'.
	fn scan_char_closure(&mut self) -> TokenAndSpan {
		use parser::token::Token::Error;
		self.expect_char(Dump, '\'');
		self.scan_char_suffix()
	}

	// Accepts char sequences for short-unicode annotation.
	// e.g. '\x7F'
	fn scan_char_ascii_hexcode(&mut self) -> TokenAndSpan {
		use parser::token::Token::Error;
		assert_eq!(self.peek(), 'x');
		match self.consume(Keep).peek() {
			/* valid unicode starting code-point */
			'0' ... '7' => match self.consume(Keep).peek() {
				/* valid unicode 2nd code-point given */
				'0' ... '9' |
				'A' ... 'F' => self.consume(Keep).scan_char_closure(),

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
	fn scan_char_unicode(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), 'u');
		self.expect_char(Keep, '{');
		let c = self.consume_counted(Keep, |c| c.is_hexdec_numeral(), |_| false);
		if 2 <= c && c <= 6 {
			self.expect_char(Keep, '}');
			self.scan_char_closure()
		}
		else {
			self.make(Token::Error)
		}
	}

	// Accepts char sequences with escape sequences as content.
	// e.g. '\n', '\\', '\'', '\x7F' or '\u{7FFFFF}', etc.
	fn scan_char_escape_sequence(&mut self) -> TokenAndSpan {
		use parser::token::Token::Error;
		assert_eq!(self.peek(), '\\');
		match self.consume(Keep).peek() {
			'0'  | /* Null */
			'n'  | /* LineFeed */
			'r'  | /* CarryReturn */
			't'  | /* Tab */
			'\\'   /* BackSlash */ => self.consume(Keep).scan_char_closure(),

			'x' => self.scan_char_ascii_hexcode(),
			'u' => self.scan_char_unicode(),
			'\'' => self.make(Error), // error: empty char literal with invalid escape sequence!
			_ => self.make(Error) // error: unknown escape sequence!
		}
	}

	fn scan_char_literal(&mut self) -> TokenAndSpan {
		use std::ascii::AsciiExt;
		use parser::token::Token::*;
		assert_eq!(self.peek(), '\'');
		match self.consume(Dump).peek() {
			'\\' => self.scan_char_escape_sequence(),
			c if c.is_ascii() => self.consume(Keep).scan_char_closure(),
			_ => self.make(Error) // error: no valid ascii!
		}
	}

	fn scan_string_literal(&mut self) -> TokenAndSpan {
		self.make(Token::Error) // TODO
	}

	fn scan_integer_and_float_suffix(&mut self) -> &mut Self {
		assert_eq!(self.peek(), '\'');
		match self.peek_2() {
			('\'', c ) if c.is_alpha() => {
				self.consume_while(Keep, |c| c.is_alpha_numeral() || c == '_');
				self
			},
			_ => self
		}
	}

	fn scan_float_suffix(&mut self) -> TokenAndSpan {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Float;
		self.scan_integer_and_float_suffix();
		self.make(Literal(Float(self.fetch_name())))
	}

	fn scan_integer_suffix(&mut self) -> TokenAndSpan {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Integer;
		self.scan_integer_and_float_suffix();
		self.make(Literal(Integer(self.fetch_name())))
	}

	fn scan_binary_literal(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), 'b');
		self.consume(Keep);
		if 1 <= self.consume_counted(Keep, |c| c.is_binary_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_octal_literal(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), 'o');
		self.consume(Keep);
		if 1 <= self.consume_counted(Keep, |c| c.is_octal_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error)
		}
	}

	fn scan_hexdec_literal(&mut self) -> TokenAndSpan {
		assert_eq!(self.peek(), 'x');
		self.consume(Keep);
		if 1 <= self.consume_counted(Keep, |c| c.is_hexdec_numeral(), |c| c == '_') {
			self.scan_integer_suffix()
		}
		else {
			self.make(Token::Error) // Error: no valid digits found for number
		}
	}

	fn scan_decimal_literal(&mut self) -> TokenAndSpan {
		assert!(self.peek().is_decimal_numeral() || self.peek() == '_');
		self.consume_while(Keep, |c| c.is_decimal_numeral() || c == '_');
		match self.peek() {
			'.' => self.scan_possible_float_literal(),
			_ => self.scan_integer_suffix()
		}
	}

	fn scan_float_literal_exponent(&mut self) -> TokenAndSpan {
		match self.peek() {
			'e' => match self.consume(Keep).peek() {
				'+' | '-' => match self.consume(Keep).peek() {
					c if c.is_decimal_numeral() => {
						self.consume_while(Keep, |c| c.is_decimal_numeral());
						self.scan_float_suffix()
					},
					_ => self.make(Token::Error) // Error: no valid digit found
				},
				_ => {
					self.make(Token::Error) // Error: '+' or '-' must follow exponent 'e'
				}
			},
			_ => {
				self.scan_float_suffix()
			}
		}
	}

	fn scan_float_literal(&mut self) -> TokenAndSpan {
		let ( p, c ) = self.peek_2();
		assert!(p == '.' && c.is_decimal_numeral());
		self.consume_n(Keep, 2);
		self.consume_while(Keep, |c| c.is_decimal_numeral() || c == '_');
		self.scan_float_literal_exponent()
	}

	/// Be careful, a '.' following a sequence of digits doesn't
	/// nessecarily mean that the following characters are part of
	/// a Float literal.
	/// E.g. a method call on an integer literal may be possible, too:
	///    42.foo()
	/// Or a range expression:
	///    0..10
	fn scan_possible_float_literal(&mut self) -> TokenAndSpan {
		use parser::token::Token::*;
		use parser::token::LiteralToken::Integer;
		assert_eq!(self.peek(), '.');
		match self.peek_2() {
			('.',  c ) if c.is_decimal_numeral() => self.scan_float_literal(),
			_ => self.make(Literal(Integer(self.fetch_name())))
		}
	}

	fn scan_number_literal(&mut self) -> TokenAndSpan {
		assert!(self.peek().is_decimal_numeral());
		match self.peek() {
			'0' => match self.consume(Keep).peek() {
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
}

// Types like the Lexer implement this trait as they are iterators
// over Tokens. Maybe it should be substituted simply with the Iterator<Token> trait.
// However, a next_token(...) method is more explicit than just next(...) and 
// it allows one to add new required methods (e.g. peek_token(...)) in future versions.
pub trait TokenStream {
    fn next_token(&mut self) -> TokenAndSpan;
}

impl<'ctx> TokenStream for Lexer<'ctx> {
	fn next_token(&mut self) -> TokenAndSpan {
		use parser::token::Token::*;
		use parser::token::DelimitToken::*;
		use parser::token::BinOpToken::*;
		use parser::token::LogicalOpToken::*;
		use parser::token::RelOpToken::*;

		// self.clear_buffer();
		self.reset_span_consumed();
		self.reset_name_buffer();

		match self.peek() {
			/* Skip whitespace */
			c if c.is_whitespace() => {
				self.consume_while(Dump, |c| c.is_whitespace());
				self.make(Whitespace)
			},

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
			'/' => match self.consume(Dump).peek() {
				'=' => self.consume(Dump).make(BinOpEq(Slash)),
				'/' => self.scan_line_comment(),
				'*' => self.scan_multi_line_comment(),
				_ => self.make(BinOp(Slash))
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
				'&' => self.consume(Dump).make(LogicalOp(AndAnd)),
				'=' => self.consume(Dump).make(BinOpEq(And)),
				_   => self.make(BinOp(And))
			},

			/* Tokens starting with '|' */
			'|' => match self.consume(Dump).peek() {
				'|' => self.consume(Dump).make(LogicalOp(OrOr)),
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

			/* Integer- and float literals and identifiers */
			c if c.is_decimal_numeral() => self.scan_number_literal(),

			/* Identifiers and keywords */
			c if c.is_alpha() => self.scan_identifier(),

			/* When end of iterator has been reached */
			_ => self.make(EndOfFile)
		}
	}
}

impl<'ctx> Iterator for Lexer<'ctx> {
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
	use parser::token::*;
	use parser::compile_context::CompileContext;
	use parser::code_map::Span;

	fn check_lexer_output_against(
		lexer: &mut Lexer,
		check_against: &[(Token, (usize, usize))]
	) {
		for &(tok, (lo, hi)) in check_against {
			let tas = TokenAndSpan {
				tok: tok,
				sp: Span::from_usize(lo, hi)
			};
			assert_eq!(lexer.next(), Some(tas));
		}
		assert_eq!(lexer.next(), None);
	}

	#[test]
	fn simple_tokens() {
		use parser::token::Token::*;
		use parser::token::DelimitToken::*;
		let ctx = CompileContext::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap("fm1", "()[]{}?;,_");
		let mut lexer = Lexer::new_for_filemap(&ctx, &fm);
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
	fn simple_comments() {
		use parser::token::Token::*;
		let ctx = CompileContext::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"//foo\n  /*bar\nbaz\n*/\n /*** //foo//***/");
		let mut lexer = Lexer::new_for_filemap(&ctx, &fm);
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
		use parser::token::Token::*;
		use parser::token::BinOpToken::*;
		use parser::token::RelOpToken::*;
		use parser::token::LogicalOpToken::*;
		let ctx = CompileContext::default();
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
			 < << <<= <= \
			 > >> >>= >="); // <= 10 columns per row!
		let mut lexer = Lexer::new_for_filemap(&ctx, &fm);
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
			(LogicalOp(AndAnd), (92, 93)),
			(Whitespace,     (94, 94)),
			(BinOpEq(And),   (95, 96)),
			(Whitespace,     (97, 99)),

			(BinOp(Or),      (100, 100)),
			(Whitespace,     (101, 101)),
			(LogicalOp(OrOr), (102, 103)),
			(Whitespace,     (104, 104)),
			(BinOpEq(Or),    (105, 106)),
			(Whitespace,     (107, 109)),

			(RelOp(LessThan),(110, 110)),
			(Whitespace,     (111, 111)),
			(BinOp(Shl),     (112, 113)),
			(Whitespace,     (114, 114)),
			(BinOpEq(Shl),   (115, 117)),
			(Whitespace,     (118, 118)),
			(RelOp(LessEq),  (119, 120)),
			(Whitespace,     (121, 121)),

			(RelOp(GreaterThan),(122, 122)),
			(Whitespace,     (123, 123)),
			(BinOp(Shr),     (124, 125)),
			(Whitespace,     (126, 126)),
			(BinOpEq(Shr),   (127, 129)),
			(Whitespace,     (130, 130)),
			(RelOp(GreaterEq), (131, 132)),
		]);
	}

	#[test]
	fn simple_whitespace() {
		use parser::token::Token::*;
		let ctx = CompileContext::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"   .\n\n\n.\r\n\r\n.\t\t\t");
		let mut lexer = Lexer::new_for_filemap(&ctx, &fm);
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
		use parser::token::Token::*;
		let ctx = CompileContext::default();
		let fm  = ctx.code_map.borrow_mut().new_filemap(
			"fm1",
			"true false \
			 alphanumeric \
			 with_underscore \
			 BiGaNdSmAlL \
			 underscores_at_the_end__ \
			 with_N0m3r5 \
			 j__u___5__T");
		let mut lexer = Lexer::new_for_filemap(&ctx, &fm);
		let sc = &ctx.string_cache;
		let name1 = sc.borrow_mut().intern("true");
		let name2 = sc.borrow_mut().intern("false");
		let name3 = sc.borrow_mut().intern("alphanumeric");
		let name4 = sc.borrow_mut().intern("with_underscore");
		let name5 = sc.borrow_mut().intern("BiGaNdSmAlL");
		let name6 = sc.borrow_mut().intern("underscores_at_the_end__");
		let name7 = sc.borrow_mut().intern("with_N0m3r5");
		let name8 = sc.borrow_mut().intern("j__u___5__T");
		check_lexer_output_against(&mut lexer, &[
			(Identifier(name1), ( 0,  3)),
			(Whitespace,        ( 4,  4)),
			(Identifier(name2), ( 5,  9)),
			(Whitespace,        (10, 10)),
			(Identifier(name3), (11, 22)),
			(Whitespace,        (23, 23)),
			(Identifier(name4), (24, 38)),
			(Whitespace,        (39, 39)),
			(Identifier(name5), (40, 50)),
			(Whitespace,        (51, 51)),
			(Identifier(name6), (52, 75)),
			(Whitespace,        (76, 76)),
			(Identifier(name7), (77, 87)),
			(Whitespace,        (88, 88)),
			(Identifier(name8), (89, 99))
		]);
	}

	// #[test]
	// fn simple_char_literal() {
	// 	use parser::token::Token::{Whitespace, Literal, EndOfFile};
	// 	use parser::token::LiteralToken::Char;
	// 	let ctx   = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(
	// 		&ctx, r"'c' '\n' '\t' '\x7F' 'a's '\n'asd0");
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'c'"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'\n'"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'\t'"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'\x7F'"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'a's"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Char(sc.borrow_mut().intern(r"'\n'asd0"))));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }

	// #[test]
	// fn simple_integral_literals() {
	// 	use parser::token::Token::*;
	// 	use parser::token::LiteralToken::Integer;
	// 	let ctx   = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(&ctx,
	// 		"0b1011_0010_0000_0001
	// 		 0o731_312_645_003
	// 		 0xFF_AE_03_95
	// 		 987654321
	// 		 0b_
	// 		 0o___
	// 		 0x_____
	// 		 0__");
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0b1011_0010_0000_0001"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0o731_312_645_003"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0xFF_AE_03_95"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("987654321"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(), Error);
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(), Error);
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(), Error);
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0__"))));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }

	// #[test]
	// fn simple_integral_suffixes() {
	// 	use parser::token::Token::*;
	// 	use parser::token::LiteralToken::Integer;
	// 	let ctx = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(&ctx,
	// 		"0b0000__0101__1111_0001'i32
	// 		 0o123_456_777_000'u64
	// 		 0xFA_01_DE_23'f32
	// 		 1234567890'hello_word"
	// 	);
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0b0000__0101__1111_0001'i32"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0o123_456_777_000'u64"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("0xFA_01_DE_23'f32"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Integer(sc.borrow_mut().intern("1234567890'hello_word"))));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }

	// #[test]
	// fn simple_float_literals() {
	// 	use parser::token::Token::*;
	// 	use parser::token::LiteralToken::Float;
	// 	let ctx = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(&ctx,
	// 		"0.0
	// 		 42.0
	// 		 0.24
	// 		 13.37
	// 		 0.00_00_1
	// 		 1.23e+12
	// 		 0.01e-07
	// 		 1_.1_");
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("0.0"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("42.0"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("0.24"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("13.37"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("0.00_00_1"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("1.23e+12"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("0.01e-07"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("1_.1_"))));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }

	// #[test]
	// fn simple_float_suffixes() {
	// 	use parser::token::Token::*;
	// 	use parser::token::LiteralToken::Float;
	// 	let ctx = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(&ctx,
	// 		"0.0'f32
	// 		 567.0'i32
	// 		 9.9999'f64
	// 		 1_2_3.3_2_1e+14'alpha
	// 		 9.87654321e-0'b37a");
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("0.0'f32"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("567.0'i32"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("9.9999'f64"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("1_2_3.3_2_1e+14'alpha"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(),
	// 		Literal(Float(sc.borrow_mut().intern("9.87654321e-0'b37a"))));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }

	// #[test]
	// fn dot_after_number_sequence() {
	// 	use parser::token::Token::*;
	// 	use parser::token::LiteralToken::{Integer};
	// 	use parser::token::DelimitToken::*;
	// 	let ctx = CompileContext::default();
	// 	let mut lexer = Lexer::new_from_str(&ctx, "42.foo() 5..10 1.e12");
	// 	let sc = &ctx.string_cache;
	// 	assert_eq!(lexer.next_token(), Literal(Integer(sc.borrow_mut().intern("42"))));
	// 	assert_eq!(lexer.next_token(), Dot);
	// 	assert_eq!(lexer.next_token(), Identifier(sc.borrow_mut().intern("foo")));
	// 	assert_eq!(lexer.next_token(), OpenDelim(Paren));
	// 	assert_eq!(lexer.next_token(), CloseDelim(Paren));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(), Literal(Integer(sc.borrow_mut().intern("5"))));
	// 	assert_eq!(lexer.next_token(), DotDot);
	// 	assert_eq!(lexer.next_token(), Literal(Integer(sc.borrow_mut().intern("10"))));
	// 	assert_eq!(lexer.next_token(), Whitespace);
	// 	assert_eq!(lexer.next_token(), Literal(Integer(sc.borrow_mut().intern("1"))));
	// 	assert_eq!(lexer.next_token(), Dot);
	// 	assert_eq!(lexer.next_token(), Identifier(sc.borrow_mut().intern("e12")));
	// 	assert_eq!(lexer.next_token(), EndOfFile);
	// }
}
