use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;

use std::ops::Add;
use std::ops::Sub;

use std::fmt;
use std::io;
use std::path::Path; // used by the FileLoader trait

use std::ops::Deref;

/// A byte offset.
/// This is used as an index into FileMaps of the owning CodeMap.
/// Keep this structure small (currently 32 bits)
/// as the AST contains many of them!
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Default)]
pub struct BytePos(pub u32);

impl BytePos {
	fn to_usize(&self) -> usize {
		let BytePos(n) = *self;
		n as usize
	}

	fn zero() -> BytePos {
		BytePos::from(0)
	}

	fn one() -> BytePos {
		BytePos::from(1)
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
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Default)]
pub struct CharPos(pub usize);

impl CharPos {
	fn to_usize(&self) -> usize {
		let CharPos(n) = *self;
		n
	}

	fn zero() -> CharPos {
		CharPos::from(0)
	}

	fn one() -> CharPos {
		CharPos::from(1)
	}
}

impl From<usize> for CharPos {
	fn from(from: usize) -> Self {
		CharPos(from as usize)
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
#[derive(Clone, Copy, Hash, PartialEq, Eq, Default)]
pub struct Span {
	pub lo: BytePos,
	pub hi: BytePos
}

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Span {{ lo: {:?}, hi: {:?} }}", self.lo, self.hi)
	}
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

	pub fn contains_pos(self, byte_pos: BytePos) -> bool {
		self.lo <= byte_pos && byte_pos <= self.hi
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

/// Identifies an offset of a multi-byte character in a FileMap
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct MultiByteChar {
    /// The absolute offset of the character in the CodeMap
    pub pos: BytePos,
    /// The number of bytes, >=2
    pub bytes: usize,
}

/// FileMap wraps FileMapData as a convenience wrapper and to disallow
/// cloning FileMapData deeply which is never needed and a very costly operation.
/// Wrapping FileMapData opens windows for creating Iterators
/// over the inner FileMapData's src String.
#[derive(Clone)]
pub struct FileMap {
	fm: Rc<FileMapData>
}

impl FileMap {
	fn new(
		filename: String,
		src: String,
		offset: BytePos
	)
		-> FileMap
	{
		use std::cmp::max;
		let end_pos = offset + BytePos::from(max(0, src.len() - 1));
		FileMap {
			fm: Rc::new(FileMapData {
				name: filename,
				src: Rc::new(src),
				span: Span::new(offset, end_pos),
				line_starts: RefCell::new(Vec::new()),
				multibyte_chars: RefCell::new(Vec::new()),
				initialized_until: Cell::new(offset)
			})
		}
	}

	pub fn iter(&self) -> FileMapIterator {
		FileMapIterator { fm: self.fm.clone(), cur_pos: self.fm.abs_offset() }
	}
}

impl Deref for FileMap {
	type Target = FileMapData;

	fn deref(&self) -> &Self::Target {
		&*(self.fm)
	}
}

/// FileMap can create FileMapIterator that iterates over all chars in its source.
/// Iterating the first time over a FileMap also initializes its line_starts
/// and multibyte_chars vectors.
pub struct FileMapIterator {
	fm: Rc<FileMapData>,
	cur_pos: BytePos
}

impl FileMapIterator {
	fn advance_cur_pos(&mut self, steps: usize) {
		self.cur_pos = self.cur_pos + BytePos::from(steps);
	}
}

/// FileMapIterator returns pairs of char and BytePos
/// wrapped in this struct for better readability.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct CharAndPos {
	pub ch: char,
	pub pos: BytePos
}

impl Iterator for FileMapIterator {
	type Item = CharAndPos;

	fn next(&mut self) -> Option<Self::Item> {
		if !self.fm.span.contains_pos(self.cur_pos) {
			return None;
		}
		match self.fm.char_at(self.cur_pos) {
			Some(next_char) => {
				let cur_pos = self.cur_pos;
				self.advance_cur_pos(char::len_utf8(next_char));
				Some(CharAndPos { ch: next_char, pos: cur_pos })
			},
			None => None
		}
	}
}

/// This class represents a source file.
/// It stores and owns its name and content.
/// Besides that it specifies a disjoint range of bytes 
/// to allow indexing into its contents
/// with the help of the SourceRange.
pub struct FileMapData {
	/// The name of this Source, by default this is the file name
	pub name: String,

	/// The content of the source file. This is an immutable String
	/// that is initialized on construction of a Source.
	pub src: Rc<String>,

	/// This is the character (or byte) range that is acceptable for
	/// for this Source to access its internal content.
	/// Source's partition the global range of bytes into disjoint chunks
	/// of byte ranges.
	pub span: Span,

	/// This is initialized upon first iteration of a FileMap
	/// and later used to speed-up construction of SourceLoc data
	/// that shows line and column count to the user (programmer)
	/// on error handling info text.
	line_starts: RefCell<Vec<BytePos>>,

    /// Locations of multi-byte characters in the source code.
	multibyte_chars: RefCell<Vec<MultiByteChar>>,

	/// The position within the FileMapData's source up to its
	/// line_starts and multibyte_chars are initialized.
	initialized_until: Cell<BytePos>
}

impl FileMapData {
	fn is_fully_initialized(&self) -> bool {
		self.initialized_until.get() == self.span.hi
	}

	fn register_line(&self, byte_pos: BytePos, ch: char) {
		if ch == '\n' {
			self.line_starts.borrow_mut().push(byte_pos);
		}
	}

	fn register_multibyte(&self, byte_pos: BytePos, ch: char) {
		let count_bytes = ch.len_utf8();
		if count_bytes >= 2 {
			self.multibyte_chars.borrow_mut().push(
				MultiByteChar {
					pos: byte_pos,
					bytes: count_bytes
				}
			);
		}
	}

	fn register_line_and_multibyte(&self, byte_pos: BytePos, ch: char) {
		// do not initialize twice!
		if self.initialized_until.get() < byte_pos {
			self.register_line(byte_pos, ch);
			self.register_multibyte(byte_pos, ch);
			self.initialized_until.set(byte_pos + BytePos::from(ch.len_utf8()) - BytePos::one());
		}
	}

	fn abs_offset(&self) -> BytePos {
		self.span.lo
	}

	fn to_relative_offset(&self, byte_pos: BytePos) -> usize {
		use std::cmp::max;
		assert!(self.span.contains_pos(byte_pos));
		max(0, (byte_pos - self.abs_offset()).to_usize())
	}

	/// Returns the nth character within this FileMap,
	/// given that the byte_pos is a correct unicode scalar code point.
	/// This method should only be called from FileMapIterator
	/// to ensure above assertion.
	fn char_at(&self, byte_pos: BytePos) -> Option<char> {
		let slice = self.src.as_str();
		let off   = self.to_relative_offset(byte_pos);
		match slice[off..].chars().next() {
			Some(next_char) => {
				self.register_line_and_multibyte(byte_pos, next_char);
				Some(next_char)
			},
			None => None
		}
	}

	fn str_from_span(&self, span: Span) -> &str {
		assert!(self.span.contains(span));
		let rel_lo = self.to_relative_offset(span.lo);
		let rel_hi = self.to_relative_offset(span.hi) + 1;
		&self.src[rel_lo .. rel_hi]
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
pub struct CodeMap {
	pub files: RefCell<Vec<FileMap>>,
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

	fn load_file(&self, path: &Path) -> io::Result<FileMap> {
		let src = try!(self.file_loader.read_file(path));
		Ok(self.new_filemap_from(path.to_str().unwrap().to_string(), src))
	}

	fn next_start_pos(&self) -> BytePos {
		match self.files.borrow().last() {
			None => BytePos::zero(),
			Some(last) => last.span.hi + BytePos::one()
		}
	}

	fn new_filemap_from(&self, filename: String, mut src: String) -> FileMap {
		// Remove utf-8 Byte Order Mark (BOM)
		if src.starts_with("\u{FEFF}") {
			src.drain(..3);
		}
		let start_pos = self.next_start_pos();
		let filemap = FileMap::new(filename, src, start_pos);
		self.files.borrow_mut().push(filemap.clone());
		filemap
	}

	fn new_filemap(&self, filename: &str, src: &str) -> FileMap {
		self.new_filemap_from(filename.to_owned(), src.to_owned())
	}

	fn enclosing_span(&self) -> Span {
		let files = self.files.borrow();
		if files.is_empty() {
			Span::default()
		}
		else {
			Span::new(
				files.first().unwrap().span.lo,
				files.last().unwrap().span.hi
			)
		}
	}

	fn contains_pos(&self, byte_pos: BytePos) -> bool {
		self.enclosing_span().contains_pos(byte_pos)
	}

	fn lookup_filemap(&self, byte_pos: BytePos) -> FileMap {
		use std::cmp::Ordering;
		let idx = self.files.borrow().binary_search_by(
			|fm| {
				if      byte_pos < fm.span.lo { Ordering::Greater }
				else if byte_pos > fm.span.hi { Ordering::Less }
				else                          { Ordering::Equal }
			}
		).unwrap();
		self.files.borrow()[idx].clone()
	}
}

/// This is only used for error handling to provide better and more
/// useful information to the user on a warning or error information.
struct Loc {
	pub source: FileMap,
	pub line: usize, // the 1-based line number within the source
	pub col: CharPos // the 0-based col'th character within the given line
}



mod tests {
	use super::*;

	fn check_iterator(
		fmit: &mut FileMapIterator,
		check_against: &[(char, usize)])
	{
		for &(ch, pos) in check_against {
			let cap = CharAndPos { ch: ch, pos: BytePos::from(pos) };
			assert_eq!(fmit.next().unwrap(), cap);
		}
		assert_eq!(fmit.next(), None);
	}

	#[test]
	fn t1() {
		let cm = CodeMap::new();
		let fm1 = cm.new_filemap("fm1", "foo\nbar baz\n\nend");
		let fm2 = cm.new_filemap("fm2", "\t\n\na\n\nb");
		let mut fmit1 = fm1.iter();
		let mut fmit2 = fm2.iter();
		assert_eq!(fm1.is_fully_initialized(), false);
		assert_eq!(fm2.is_fully_initialized(), false);
		assert_eq!(fm1.line_starts.borrow().len(), 0);
		assert_eq!(fm2.line_starts.borrow().len(), 0);
		assert_eq!(fm1.name, "fm1");
		assert_eq!(fm2.name, "fm2");
		assert_eq!(fm1.span, Span::from_usize( 0, 15));
		assert_eq!(fm2.span, Span::from_usize(16, 22));
		assert_eq!(fm1.abs_offset(), BytePos::from(0));
		assert_eq!(fm2.abs_offset(), BytePos::from(16));
		check_iterator(&mut fmit1, &[
			('f', 0), ('o', 1), ('o', 2), ('\n', 3),
			('b', 4), ('a', 5), ('r', 6), (' ', 7), ('b', 8), ('a', 9), ('z', 10), ('\n', 11),
			('\n', 12),
			('e', 13), ('n', 14), ('d', 15)
		]);
		assert_eq!(fm1.str_from_span(Span::from_usize(0, 2)), "foo");
		assert_eq!(fm1.str_from_span(fm1.span), "foo\nbar baz\n\nend");
		assert_eq!(fm1.is_fully_initialized(), true);
		assert_eq!(fm1.line_starts.borrow().len(), 3);
		assert_eq!(fm1.line_starts.borrow()[0], BytePos::from(3));
		assert_eq!(fm1.line_starts.borrow()[1], BytePos::from(11));
		assert_eq!(fm1.line_starts.borrow()[2], BytePos::from(12));
		assert_eq!(fm1.multibyte_chars.borrow().len(), 0);
		check_iterator(&mut fmit2, &[
			('\t', 16), ('\n', 17),
			('\n', 18),
			('a', 19), ('\n', 20),
			('\n', 21),
			('b', 22)
		]);
		assert_eq!(fm2.is_fully_initialized(), true);
		assert_eq!(fm2.line_starts.borrow().len(), 4);
		assert_eq!(fm2.line_starts.borrow()[0], BytePos::from(17));
		assert_eq!(fm2.line_starts.borrow()[1], BytePos::from(18));
		assert_eq!(fm2.line_starts.borrow()[2], BytePos::from(20));
		assert_eq!(fm2.line_starts.borrow()[3], BytePos::from(21));
		assert_eq!(fm2.multibyte_chars.borrow().len(), 0);
		let fm3 = cm.lookup_filemap(BytePos::from(0));
		let fm4 = cm.lookup_filemap(BytePos::from(15));
		let fm5 = cm.lookup_filemap(BytePos::from(16));
		let fm6 = cm.lookup_filemap(BytePos::from(22));
		assert_eq!(fm3.name, fm1.name);
		assert_eq!(fm4.name, fm1.name);
		assert_eq!(fm5.name, fm2.name);
		assert_eq!(fm6.name, fm2.name);
		assert_eq!(cm.contains_pos(BytePos::from(0)), true);
		assert_eq!(cm.contains_pos(BytePos::from(8)), true);
		assert_eq!(cm.contains_pos(BytePos::from(16)), true);
		assert_eq!(cm.contains_pos(BytePos::from(22)), true);
		assert_eq!(cm.contains_pos(BytePos::from(23)), false);
		assert_eq!(cm.contains_pos(BytePos::from(100)), false);
	}
}