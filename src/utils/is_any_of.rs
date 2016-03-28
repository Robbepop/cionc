pub trait IsAnyOf<T : PartialEq> {
	fn is_any_of(&self, options: &[T]) -> bool;
	fn is_none_of(&self, options: &[T]) -> bool;
} 

impl<T> IsAnyOf<T> for T where T: PartialEq {
	fn is_any_of(&self, options: &[T]) -> bool {
		options.iter().any(|v| *v == *self)
	}

	fn is_none_of(&self, options: &[T]) -> bool {
		!self.is_any_of(options)
	}
}

#[cfg(test)]
mod tests {
	use is_any_of::IsAnyOf;

	#[test]
	fn test_is_any_of() {
		let arr = &[1,2,3];
		assert!( 1.is_any_of(arr));
		assert!( 3.is_any_of(arr));
		assert!(!4.is_any_of(arr));
		assert!(!1.is_any_of(&[]));
		assert!(!4.is_any_of(&[]));
		assert!( 'm'.is_any_of(&['n', '\n', 'm', ' ']));
		assert!(!'o'.is_any_of(&['r', '\r', 'p', ' ']));
	}

	#[test]
	fn test_is_non_of() {
		assert!(!1.is_none_of(&[1,2,3]));
		assert!(!3.is_none_of(&[1,2,3]));
		assert!( 4.is_none_of(&[1,2,3]));
		assert!( 1.is_none_of(&[]));
		assert!( 4.is_none_of(&[]));
	}
}