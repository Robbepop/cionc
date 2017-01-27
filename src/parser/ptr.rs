use std::ops::{Deref, DerefMut};

struct P<T>(Box<T>);

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
