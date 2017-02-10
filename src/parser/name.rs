use string_interner::{NonNegative};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(u32);

impl NonNegative for Name {}

impl From<usize> for Name {
	fn from(val: usize) -> Self { Name(val as u32) }
}

impl From<Name> for usize {
	fn from(name: Name) -> usize { name.0 as usize }
}
