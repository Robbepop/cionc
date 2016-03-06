// Token represents all possible Token types that can be created by
// an instance that implements the TokenStream trait.
// Some Tokens as identifiers and some literals carry some more information
// for later use in the parser.
// The tokens and their layout used in this simple implementation
// are based on the Rustc token types.

use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOpToken {
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    Caret,   // ^
    And,     // &
    Or,      // |
    Shl,     // <<
    Shr      // >>
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum RelOpToken {
    EqEq,        // ==
    NotEq,       // !=
    LessThan,    // <
    LessEq,      // <=
    GreaterThan, // >
    GreaterEq    // >=
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum LogicalOpToken {
    AndAnd, // &&
    OrOr    // ||
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum DelimitToken {
    Paren,   // ( or )
    Bracket, // [ or ]
    Brace,   // { or }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum LiteralToken {
    Bool(Rc<String>),    // e.g. true or false
    Char(Rc<String>),    // e.g. 'a'
    Integer(Rc<String>), // e.g. 5, 42, 1337, 0
    Float(Rc<String>),   // e.g. 0.1, 5.0, 13.37, 0.0
    String(Rc<String>)   // e.g. "Hello, World!"
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    /* Logical operators, e.g. && or || */
    LogicalOp(LogicalOpToken),
    /* Binary operators compatible with assignment, e.g. +, - */
    BinOp(BinOpToken),
    /* Binary assignment operators, e.g. +=, -= */
    BinOpEq(BinOpToken),
    /* Relational operators, e.g. <, <=, >, >=, ==, != */
    RelOp(RelOpToken),

    /* An opening delimiter, e.g. { or ( or [ */
    OpenDelim(DelimitToken),

    /* A closing delimiter, e.g. } or ) or ] */
    CloseDelim(DelimitToken),

    /* Identifiers with their given name */
    Identifier(Rc<String>),
    /* Literal token, e.g. an integer, float or string literal */
    Literal(LiteralToken),

    /* Special tokens */
    Eq,          // =
    Colon,       // :
    SemiColon,   // ;
    ColonColon,  // ::
    Dot,         // .
    DotDot,      // ..
    DotDotDot,   // ...
    Comma,       // ,
    Exclamation, // !
    Question,    // ?
    Arrow,       // ->
    FatArrow,    // =>
    Underscore,  // _

    /* Junk tokens which the parser doesn't require in order to parse the program. */
    Whitespace,
    Comment,

    /* End of file (EOF) token indicating the end of stream for parsing */
    EndOfFile,

    /* Token indicating that an errornous sequence has been found */
    Error
}

use parser::token::Token::{Identifier, Literal};
use parser::token::LiteralToken::{Char, Bool, Integer, Float};

impl Token {
    pub fn bool_literal_from_str(slice: &str) -> Token {
        Literal(Bool(Rc::new(String::from(slice))))
    }

    pub fn char_literal_from_str(slice: &str) -> Token {
        Literal(Char(Rc::new(slice.to_owned().to_string())))
    }

    pub fn integer_literal_from_str(slice: &str) -> Token {
        Literal(Integer(Rc::new(String::from(slice))))
    }

    pub fn float_literal_from_str(slice: &str) -> Token {
        Literal(Float(Rc::new(String::from(slice))))
    }

    pub fn string_literal_from_str(slice: &str) -> Token {
        Literal(LiteralToken::String(Rc::new(slice.to_owned().to_string())))
    }

    pub fn identifier_from_str(slice: &str) -> Token {
        Identifier(Rc::new(String::from(slice)))
    }
}