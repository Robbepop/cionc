use std::cell::RefCell;
use std::rc::Rc;
use std::ops::Deref;

use name::Name;
use string_interner::StringInterner;
use code_map::CodeMap;

// This type is the root to manage several subcomponents.
// In later revisions it has a StringTable, a SymbolTable, an ErrorHandler and more
// important compile modules that other compile subcomponents like the Lexer or Parser
// can use.
// It has methods to get references to its members so types like Lexer and Parser just
// have to have one member of this type in order to access all of its utility members.
#[derive(Clone)]
pub struct ParseSess {
	data: Rc<ParseSessData>
}

pub struct ParseSessData {
	pub symbol_table: RefCell<StringInterner<Name>>,
	pub code_map    : RefCell<CodeMap>
}

impl ParseSessData {
	pub fn new() -> Self {
		Self{
			symbol_table: RefCell::new(StringInterner::new()),
			code_map    : RefCell::new(CodeMap::new())
		}
	}
}

impl ParseSess {
	pub fn new() -> Self {
		Self{
			data: Rc::new(ParseSessData::new())
		}
	}
}

impl Deref for ParseSess {
	type Target = ParseSessData;

	fn deref(&self) -> &Self::Target {
		&self.data
	}
}
