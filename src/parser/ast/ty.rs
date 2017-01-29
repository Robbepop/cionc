use ast::node_id::NodeId;

pub struct Ty {
	id  : NodeId,
	kind: TyKind
}

pub enum TyKind {
	None,
	Infer,
	ImplicitSelf,
	Ref(RefTy),
	Slice(SliceTy),
	Array(ArrayTy),
	Tuple(TupleTy),
	Fn(FnTy),
	Path(PathTy)
}

pub struct RefTy {

}

pub struct SliceTy {

}

pub struct ArrayTy {

}

pub struct TupleTy {

}

pub struct FnTy {

}

pub struct PathTy {

}
