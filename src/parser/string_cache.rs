use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use std::borrow::Borrow;
use std::cmp::Ordering;
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

impl Ord for RcBoxStr {
    fn cmp(&self, other: &RcBoxStr) -> Ordering {
        self[..].cmp(&other[..])
    }
}

impl fmt::Debug for RcBoxStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Debug;
        self[..].fmt(f)
    }
}

impl fmt::Display for RcBoxStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Display;
        self[..].fmt(f)
    }
}

impl Borrow<str> for RcBoxStr {
    fn borrow(&self) -> &str {
        &self.string[..]
    }
}

impl Deref for RcBoxStr {
    type Target = str;

    fn deref(&self) -> &str { &self.string[..] }
}



#[derive(Default)]
pub struct StringCache {
	map: RefCell<HashMap<RcBoxStr, Name>>,
	names: RefCell<Vec<RcBoxStr>>
}

impl StringCache {
	pub fn intern(&self, content: &str) -> Name {
		match self.map.borrow_mut().get(content) {
			Some(&id) => return id,
			None      => ()
		}
		let new_id = Name(self.len() as u32);
		let new_item = RcBoxStr::new(content);
		self.map.borrow_mut().insert(new_item.clone(), new_id);
		self.names.borrow_mut().push(new_item);
		new_id
	}

	pub fn get(&self, id: Name) -> RcBoxStr {
		(self.names.borrow()[id.0 as usize]).clone()
	}

	pub fn len(&self) -> usize {
		self.names.borrow().len()
	}
}

mod tests {
	use super::*;

	#[test]
	fn simple_test() {
		let cache = StringCache::default();
		assert_eq!(cache.intern("foo"), Name(0));
		assert_eq!(cache.intern("bar"), Name(1));
		assert_eq!(cache.intern("baz"), Name(2));
		assert_eq!(cache.intern("bat"), Name(3));
		assert_eq!(cache.intern("bam"), Name(4));
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
	}
}