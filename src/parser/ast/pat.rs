use ast::node_id::NodeId;

pub struct Pat {
	pub id: NodeId,
	pub kind: PatKind
}

pub enum PatKind {
	/// a wildcard pattern (`_`)
	Wild,

	/// a literal, e.g. `5`
	Lit(LitPat),

	/// a reference pattern, e.g. `&mut (a, b)`
	Ref(RefPat),

	/// a tuple, e.g. `(a, b)`
	Tuple(TuplePat),

	/// a struct, e.g. `Point{x, y, ..}`
	Struct(StructPat),

	/// a tuple struct, e.g. `Some(5)`
	TupleStruct(TupleStructPat),

	/// a inclusive or exclusive range, e.g. `1..2` or `0...10`
	Range(RangePat),

	/// a slice pattern, e.g. `[a, b, ..i, y, z]`
	Slice(SlicePat),

	/// a path pattern, e.g. `A::B::a`
	Path(PathPat)
}

pub struct LitPat {

}

pub struct RefPat {

}

pub struct TuplePat {

}

pub struct StructPat {

}

pub struct TupleStructPat {

}

pub struct RangePat {

}

pub struct SlicePat {

}

pub struct PathPat {

}