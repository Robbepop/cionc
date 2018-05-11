use std::ops::{Deref, DerefMut};

/// Used for indirection within the abstract syntax tree (AST).
/// 
/// This is currently just a simple wrapper around Box<T> but
/// this may change in future.
pub struct P<T>(Box<T>);

impl<T> Deref for P<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl<T> DerefMut for P<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}
