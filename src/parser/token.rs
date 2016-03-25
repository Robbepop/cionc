// Token represents all possible Token types that can be created by
// an instance that implements the TokenStream trait.
// Some Tokens as identifiers and some literals carry some more information
// for later use in the parser.
// The tokens and their layout used in this simple implementation
// are based on the Rustc token types.

use std::rc::Rc;

use parser::string_cache::Name;

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

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum LiteralToken {
    // Byte(Name),      // e.g. b'a'
    Char(Name),      // e.g. 'a'
    Integer(Name),   // e.g. 5, 42, 1337, 0
    Float(Name),     // e.g. 0.1, 5.0, 13.37, 0.0
    String(Name),    // e.g. "Hello, World!"
    // RawString(Name), // e.g. r#"Hello, World!"#
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
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
    Identifier(Name),

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
    Error,
}