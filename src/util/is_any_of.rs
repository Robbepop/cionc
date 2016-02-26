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
