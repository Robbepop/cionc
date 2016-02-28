use std::cell::RefCell;
use std::cell::RefMut;
use parser::string_table::StringTable;

// This type is the root to manage several subcomponents.
// In later revisions it has a StringTable, a SymbolTable, an ErrorHandler and more
// important compile modules that other compile subcomponents like the Lexer or Parser
// can use.
// It has methods to get references to its members so types like Lexer and Parser just
// have to have one member of this type in order to access all of its utility members.

use std::ops::DerefMut;

#[derive(Default)]
pub struct CompileContext {
	string_table: RefCell<StringTable>
}

impl CompileContext {
	pub fn get_string_table<'ctx>(&'ctx self) -> RefMut<'ctx, StringTable> {
		self.string_table.borrow_mut()
	}
}
