use std::cell::RefCell;

use string_cache::StringCache;
use code_map::CodeMap;

// This type is the root to manage several subcomponents.
// In later revisions it has a StringTable, a SymbolTable, an ErrorHandler and more
// important compile modules that other compile subcomponents like the Lexer or Parser
// can use.
// It has methods to get references to its members so types like Lexer and Parser just
// have to have one member of this type in order to access all of its utility members.

#[derive(Default)]
pub struct CompileContext {
	pub string_cache: RefCell<StringCache>,
	pub code_map: RefCell<CodeMap>
}
