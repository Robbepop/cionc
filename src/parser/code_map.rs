use std::rc::Rc;
use std::cell::RefCell;

use std::ops::Add;
use std::ops::Sub;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
struct BytePos(pub u32);

impl BytePos {
    fn from_usize(n: usize) -> BytePos {
    	BytePos(n as u32)
    }

	fn to_usize(&self) -> usize {
		let BytePos(n) = *self;
		n as usize
	}
}

impl Sub<BytePos> for BytePos {
	type Output = Self;

	fn sub(self, rhs: BytePos) -> BytePos {
		BytePos::from_usize(self.to_usize() - rhs.to_usize())
	}
}

impl Add<BytePos> for BytePos {
	type Output = Self;

	fn add(self, rhs: BytePos) -> BytePos {
		BytePos::from_usize(self.to_usize() + rhs.to_usize())
	}
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
struct CharPos(pub u32);

impl From<usize> for BytePos {
	fn from(from: usize) -> Self {
		BytePos(from as u32)
	}
}

/// This represents a range of bytes within the contents of a Source.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
struct ByteRange{
	pub begin: BytePos,
	pub end: BytePos
}

impl ByteRange {
	pub fn new(b: BytePos, e: BytePos) -> Self {
		ByteRange{begin: b, end: e}
	}

	pub fn from(b: usize, e: usize) -> Self {
		ByteRange::new(BytePos::from(b), BytePos::from(e))
	}
}

/// This class represents a source file.
/// It stores and owns its name and content.
/// Besides that it specifies a disjoint range of bytes 
/// to allow indexing into its contents
/// with the help of the SourceRange.
struct Source {
	/// The name of this Source, by default this is the file name
	pub file_name: String,

	/// The content of the source file. This is an immutable String
	/// that is initialized on construction of a Source.
	pub content: Rc<String>,

	/// This is the character (or byte) range that is acceptable for
	/// for this Source to access its internal content.
	/// Source's partition the global range of bytes into disjoint chunks
	/// of byte ranges.
	range: ByteRange,

	/// This is initialized upon construction of a Source
	/// and later used to speed-up construction of SourceLoc data
	/// that shows line and column count to the user (programmer)
	/// on error handling info text.
	line_starts: Vec<BytePos>
}

impl Source {
	// fn new(global_offset: BytePos, file_name: String, content: String) -> Source {
		// TODO		
	// }
	fn content_from_range(&self, byte_range: ByteRange) -> String {
		let off = self.range.begin;
		self.content[
			(byte_range.begin - off).to_usize() .. (byte_range.end - off).to_usize()
		].to_string()
	}
}

/// This is the managing unit of all Source files within the program.
/// It is used as a builder for new Sources and manages their disjoint
/// byte ranges in order to index into their contents more easily.
struct SourceRange {
	pub sources: RefCell<Vec<Rc<Source>>>
}

/// This is only used for error handling to provide better and more
/// useful information to the user on a warning or error information.
struct SourceLoc {
	pub source: Rc<Source>,
	pub line: usize, // the 1-based line number within the source
	pub col: CharPos // the 0-based col'th character within the given line
}