use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use std::borrow::Borrow;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Name(u32);

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd)]
pub struct RcBoxStr {
    string: Rc<Box<str>>,
}

impl RcBoxStr {
    pub fn new(string: &str) -> RcBoxStr {
        RcBoxStr {
            string: Rc::new(string.to_string().into_boxed_str()),
        }
    }
}

impl fmt::Debug for RcBoxStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self[..].fmt(f)
    }
}

impl fmt::Display for RcBoxStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self[..].fmt(f)
    }
}

impl Borrow<str> for RcBoxStr {
    fn borrow(&self) -> &str {
        self
    }
}

impl Deref for RcBoxStr {
    type Target = str;

    fn deref(&self) -> &str { &self.string[..] }
}


#[derive(Default)]
pub struct StringCache {
	map  : HashMap<RcBoxStr, Name>,
	names: Vec<RcBoxStr>
}

impl StringCache {
	pub fn prefill(&mut self, contents: &[&str]) -> Vec<Name> {
		contents
			.iter()
			.map(|s| self.intern(s))
			.collect()
	}

	pub fn intern(&mut self, content: &str) -> Name {
		match self.map.get(content) {
			Some(&id) => return id,
			None      => {
				let (rcstr, name) = self.gensym(content);
				self.map.insert(rcstr, name);
				name
			}
		}
	}

	fn gensym(&mut self, content: &str) -> (RcBoxStr, Name) {
		let rcstr = RcBoxStr::new(content);
		let name  = Name(self.len() as u32);
		self.names.push(rcstr.clone());
		(rcstr, name)
	}

	pub fn get(&self, id: Name) -> RcBoxStr {
		self.names[id.0 as usize].clone()
	}

	pub fn len(&self) -> usize {
		self.names.len()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn simple_test() {
		let mut cache = StringCache::default();
		assert_eq!(cache.len(), 0);
		assert_eq!(cache.intern("foo"), Name(0));
		assert_eq!(cache.intern("bar"), Name(1));
		assert_eq!(cache.intern("baz"), Name(2));
		assert_eq!(cache.intern("bat"), Name(3));
		assert_eq!(cache.intern("bam"), Name(4));
		assert_eq!(cache.len(), 5);
		assert_eq!(cache.get(Name(0)), RcBoxStr::new("foo"));
		assert_eq!(cache.get(Name(1)), RcBoxStr::new("bar"));
		assert_eq!(cache.get(Name(2)), RcBoxStr::new("baz"));
		assert_eq!(cache.get(Name(3)), RcBoxStr::new("bat"));
		assert_eq!(cache.get(Name(4)), RcBoxStr::new("bam"));
		assert_eq!(cache.intern("foo"), Name(0));
		assert_eq!(cache.intern("bar"), Name(1));
		assert_eq!(cache.intern("baz"), Name(2));
		assert_eq!(cache.intern("bat"), Name(3));
		assert_eq!(cache.intern("bam"), Name(4));
		assert_eq!(cache.len(), 5);
		assert_eq!(cache.intern("new"), Name(5)); // new item!
		assert_eq!(cache.len(), 6);
	}
}