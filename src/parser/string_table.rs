use std::collections::HashMap;

use std::rc::Rc;

/// This class represents a collection of cached strings.

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct StringTable {
	// This should actually use a HashSet<String> instead of a HashMap<String,String>.
	// However, the current standard API makes it impossible for this use-case
	// because the Entry API works only on values and not keys.
	entries: HashMap<String, Rc<String>>
}

impl StringTable {
	pub fn get_or_insert<'table>(&'table mut self, key: &String) -> Rc<String> {
		self.entries.entry(key.clone()).or_insert_with(|| Rc::new(key.clone())).clone()
	}
}