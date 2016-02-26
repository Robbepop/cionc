pub use super::token::Token;

// Types like the Lexer implement this trait as they are iterators
// over Tokens. Maybe it should be substituted simply with the Iterator<Token> trait.
// However, a next_token(...) method is more explicit than just next(...) and 
// it allows one to add new required methods (e.g. peek_token(...)) in future versions.
pub trait TokenStream<'a> {
    fn next_token(&mut self) -> Token<'a>;
}