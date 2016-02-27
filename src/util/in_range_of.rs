pub trait InRangeOf<T : PartialEq> {
	fn in_range_of(&self, lower_bound: T, upper_bound: T) -> bool;
} 

impl<T> InRangeOf<T> for T where T: PartialEq + PartialOrd {
	fn in_range_of(&self, lower_bound: T, upper_bound: T) -> bool {
		assert!(lower_bound <= upper_bound);
		lower_bound <= *self && *self <= upper_bound
	}
}