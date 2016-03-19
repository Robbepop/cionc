use std::rc::Rc;
use std::cell::RefCell;

use std::ops::Add;
use std::ops::Sub;

use std::io;
use std::path::Path; // used by the FileLoader trait

/// A byte offset.
/// This is used as an index into FileMaps of the owning CodeMap.
/// Keep this structure small (currently 32 bits)
/// as the AST contains many of them!
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Default)]
struct BytePos(pub u32);

impl BytePos {
	fn to_usize(&self) -> usize {
		let BytePos(n) = *self;
		n as usize
	}
}

impl From<usize> for BytePos {
	fn from(from: usize) -> Self {
		BytePos(from as u32)
	}
}

impl Sub<BytePos> for BytePos {
	type Output = Self;

	fn sub(self, rhs: BytePos) -> Self::Output {
		BytePos::from(self.to_usize() - rhs.to_usize())
	}
}

impl Add<BytePos> for BytePos {
	type Output = Self;

	fn add(self, rhs: BytePos) -> Self::Output {
		BytePos::from(self.to_usize() + rhs.to_usize())
	}
}

/// A character offset within a FileMap.
/// Because of multibyte utf8 characters, a byte offset (BytePos)
/// is not equivalent to a character offset.
/// The CodeMap will convert instances of BytePos into
/// instances of CharPos as necessary.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
struct CharPos(pub usize);

impl CharPos {
	fn to_usize(&self) -> usize {
		let CharPos(n) = *self;
		n
	}
}

impl From<usize> for CharPos {
	fn from(from: usize) -> Self {
		CharPos(from as usize)
	}
}

impl From<BytePos> for CharPos {
	fn from(from: BytePos) -> Self {
		CharPos::from(from.to_usize())
	}
}

impl Sub<CharPos> for CharPos {
	type Output = Self;

	fn sub(self, rhs: CharPos) -> Self::Output {
		CharPos::from(self.to_usize() - rhs.to_usize())
	}
}

impl Add<CharPos> for CharPos {
	type Output = Self;

	fn add(self, rhs: CharPos) -> Self::Output {
		CharPos::from(self.to_usize() + rhs.to_usize())
	}
}

/// Represents a span of BytePos from lo (low) to hi (high).
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Span {
	pub lo: BytePos,
	pub hi: BytePos
}

impl Span {
	pub fn new(lo: BytePos, hi: BytePos) -> Self {
		Span {lo: lo, hi: hi}
	}

	pub fn from_usize(lo: usize, hi: usize) -> Self {
		Span {lo: BytePos::from(lo), hi: BytePos::from(hi)}
	}

	pub fn contains(self, other: Span) -> bool {
		self.lo <= other.lo && other.hi <= self.hi
	}

	pub fn overlaps_with(self, other: Span) -> bool {
		self.lo <= other.lo && self.hi > other.lo ||
		other.lo <= self.lo && other.hi > self.lo
	}

	pub fn merge(self, other: Span) -> Option<Span> {
		use std::cmp::{min, max};
		if self.overlaps_with(other) {
			let new_lo = min(self.lo, other.lo);
			let new_hi = max(self.hi, other.hi);
			Some(Span {lo: new_lo, hi: new_hi})
		}
		else {
			None
		}
	}
}

/// This class represents a source file.
/// It stores and owns its name and content.
/// Besides that it specifies a disjoint range of bytes 
/// to allow indexing into its contents
/// with the help of the SourceRange.
struct FileMap {
	/// The name of this Source, by default this is the file name
	pub name: String,

	/// The content of the source file. This is an immutable String
	/// that is initialized on construction of a Source.
	pub src: Rc<String>,

	/// This is the character (or byte) range that is acceptable for
	/// for this Source to access its internal content.
	/// Source's partition the global range of bytes into disjoint chunks
	/// of byte ranges.
	span: Span,

	/// This is initialized upon construction of a Source
	/// and later used to speed-up construction of SourceLoc data
	/// that shows line and column count to the user (programmer)
	/// on error handling info text.
	line_starts: RefCell<Vec<BytePos>>
}

impl FileMap {
	fn content_from_span(&self, span: Span) -> String {
		let off = self.span.lo;
		self.src[
			(span.lo - off).to_usize() .. (span.hi - off).to_usize()
		].to_string()
	}
}

/// An abstraction over the file system operations required by the Parser.
pub trait FileLoader {
	/// Query the existence of a file.
	fn file_exists(&self, path: &Path) -> bool;

	/// Read the contents of an UTF-8 encoded file into memory.
	fn read_file(&self, path: &Path) -> io::Result<String>;
}

/// A FileLoader that uses std::fs to load concrete files.
pub struct ConcreteFileLoader;

impl FileLoader for ConcreteFileLoader {
	fn file_exists(&self, path: &Path) -> bool {
		use std::fs;
		fs::metadata(path).is_ok()
	}

	fn read_file(&self, path: &Path) -> io::Result<String> {
		use std::fs;
		use std::io::Read;
		let mut src = String::new();
		try!(
			try!(fs::File::open(path)).read_to_string(&mut src)
		);
		Ok(src)
	}
}

/// This is the managing unit of all Source files within the program.
/// It is used as a builder for new Sources and manages their disjoint
/// byte ranges in order to index into their contents more easily.
struct CodeMap {
	pub files: RefCell<Vec<Rc<FileMap>>>,
	file_loader: Box<FileLoader>
}

impl CodeMap {
	pub fn new() -> CodeMap {
		CodeMap {
			files: RefCell::new(Vec::new()),
			file_loader: Box::new(ConcreteFileLoader)
		}
	}

	pub fn new_with_file_loader(file_loader: Box<FileLoader>) -> CodeMap {
		CodeMap {
			files: RefCell::new(Vec::new()),
			file_loader: file_loader
		}
	}

	/// Just a small helper-routine
	fn file_exists(&self, path: &Path) -> bool {
		self.file_loader.file_exists(path)
	}

	fn load_file(&self, path: &Path) -> io::Result<Rc<FileMap>> {
		let src = try!(self.file_loader.read_file(path));
		Ok(self.new_filemap(path.to_str().unwrap().to_string(), src))
	}

	fn next_start_pos(&self) -> usize {
		match self.files.borrow().last() {
			None => 0,
			// Add one so there is some space between files.
			// This lets us distinguish positions in the codemap,
			// even in the presence of zero-length files.
			Some(last) => last.span.hi.to_usize() + 1
		}
	}

	fn new_filemap(&self, filename: String, mut src: String) -> Rc<FileMap> {
		// Remove utf-8 Byte Order Mark (BOM)
		if src.starts_with("\u{FEFF}") {
			src.drain(..3);
		}
		let start_pos = self.next_start_pos();
		let end_pos   = start_pos + src.len();
		let filemap   = Rc::new(FileMap {
			name: filename,
			src: Rc::new(src),
			span: Span::from_usize(start_pos, end_pos),
			line_starts: RefCell::new(Vec::new())
		});
		self.files.borrow_mut().push(filemap.clone());
		filemap
	}
}

/// This is only used for error handling to provide better and more
/// useful information to the user on a warning or error information.
struct Loc {
	pub source: Rc<FileMap>,
	pub line: usize, // the 1-based line number within the source
	pub col: CharPos // the 0-based col'th character within the given line
}