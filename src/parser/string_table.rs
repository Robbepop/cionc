use std::ops::Deref;

/// This class represents a collection of cached strings.
/// It works just like a Set of Strings,
/// however, due to current unstable annotations to HashSet::get(...)
/// and BTreeSet::get(...) it is impossible to implement on that types.
///
/// So the current implementation will be replaced once the standard
/// annotates the mentioned method (or more -> get_or_insert(...)
/// as already proposed) as stable.

#[derive(Default, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StringTable {
	/* I use Box<String> because of possible iterator invalidation
	   in case the Vec is growing in size and reallocating memory. */
	entries: Vec<Box<String>>
}

impl StringTable {
	/// This method inters the given String into the StringTable
	/// if it does not exist and returns a reference to the newly created
	/// and boxed String or to the already stored instance.
	pub fn get_or_insert(&mut self, string: String) -> &str {
		let boxed = Box::new(string);
		if !self.entries.contains(&boxed) {
			self.entries.push(boxed);
		}
		self.entries.last().unwrap()
	}

	// This code should work (in my opinion), but the borrow checker prevents it
	// from compiling because of an immutable borrow of self.
	// How can I fix things like that in general?
	// I guess I have to restructure my project or at least how this type works.
	//
	// pub fn get_or_insert(&mut self, string: String) -> &str {
	// 	match self.entries.iter().find(|&b| (*b).deref() == &string) {
	// 		Some(s) => s,
	// 		None => {
	// 			self.entries.push(Box::new(string));
	// 			self.entries.last().unwrap().deref()
	// 		}
	// 	}
	// }
}
