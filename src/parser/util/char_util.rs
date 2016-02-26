use ::util::in_range_of::*;

// This trait is just a very simply utility for working with characters in the Lexer
// so that checks like is_whitespace can be called via method call syntax
// as c.is_whitespace() just looks plainly better than is_whitespace(c).
//
// However, I have already read that it is bad code smell in Rust to implement
// custom traits for native types - I am asking why and what should I do instead?
// Create a wrapper for char and implement these methods there?

pub trait CharProperties {
	fn is_whitespace(&self) -> bool;
	fn is_binary_numeral(&self) -> bool;
	fn is_octal_numeral(&self) -> bool;
	fn is_decimal_numeral(&self) -> bool;
	fn is_hexdec_numeral(&self) -> bool;
	fn is_alpha(&self) -> bool;
	fn is_alpha_numeral(&self) -> bool;
}

impl CharProperties for char {
	fn is_whitespace(&self) -> bool {
		match *self {
			' '  |
			'\t' |
			'\n' |
			'\r' => true,
			_    => false
		}
	}

	fn is_binary_numeral(&self) -> bool {
		*self == '0' ||
		*self == '1'
	}

	fn is_octal_numeral(&self) -> bool {
		self.in_range_of('0','7')
	}

	fn is_decimal_numeral(&self) -> bool {
		self.in_range_of('0','9')
	}

	fn is_hexdec_numeral(&self) -> bool {
		self.is_decimal_numeral() ||
		self.in_range_of('a','z') ||
		self.in_range_of('A','Z')
	}

	fn is_alpha(&self) -> bool {
		self.in_range_of('a','z') ||
		self.in_range_of('A','Z')
	}

	fn is_alpha_numeral(&self) -> bool {
		self.is_alpha() || self.is_decimal_numeral()
	}
}

mod tests {
	use super::CharProperties;

	#[test]
	fn test_whitespace() {
		assert!(' '.is_whitespace());
		assert!('\n'.is_whitespace());
		assert!('\r'.is_whitespace());
		assert!('\t'.is_whitespace());
	}

	#[test]
	fn test_numeric() {
		let characters = &[
			'0','1','2','3','4','5','6','7','8','9',
			'a','b','c','d','e','f',
			'A','B','C','D','E','F'];
		for c in characters {
			println!("{}, ", c);
			println!("	binary");
			assert_eq!(c.is_digit(2), c.is_binary_numeral());
			println!("	octal");
			assert_eq!(c.is_digit(8), c.is_octal_numeral());
			println!("	decimal");
			assert_eq!(c.is_digit(10), c.is_decimal_numeral());
			println!("	hexdec");
			assert_eq!(c.is_digit(16), c.is_hexdec_numeral());
		}
	}
}