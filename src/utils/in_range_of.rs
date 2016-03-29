pub trait InRangeOf<T : PartialEq> {
	fn in_range_of(&self, lower_bound: T, upper_bound: T) -> bool;
} 

impl<T> InRangeOf<T> for T where T: PartialEq + PartialOrd {
	fn in_range_of(&self, lower_bound: T, upper_bound: T) -> bool {
		assert!(lower_bound <= upper_bound);
		lower_bound <= *self && *self <= upper_bound
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test() {
		assert!( 1.in_range_of(1,3));
		assert!( 2.in_range_of(1,3));
		assert!( 3.in_range_of(1,3));
		assert!(!4.in_range_of(1,3));
		assert!('b'.in_range_of('a', 'z'));
		assert!('B'.in_range_of('A', 'Z'));
	}

	#[test]
	#[should_panic]
	fn invalid_lower_upper() {
		assert!('c'.in_range_of('z', 'a'));
	}
}