use ast::node_id::NodeId;
use ast::ptr::P;

pub struct Ty {
	id  : NodeId,
	kind: TyKind
}

pub enum TyKind {
	/// similar to `void´ in C/C++
	None,

	/// used for lamdas and closures when the return type is omitted
	Infer,

	/// used for the self type of methods
	ImplicitSelf,

	/// e.g. ref Int
	Ref(RefTy),

	/// variable length array, e.g. [F32]
	Slice(SliceTy),

	/// fixed-length array, e.g. [Int; 3]
	Array(ArrayTy),

	/// e.g. `(Bool, I32)´ or special syntax `(F64,)´ for single-element tuples
	Tuple(TupleTy),

	/// e.g. fn(F32, F32) -> F32
	Fn(FnTy),

	/// e.g. single elements `Int´ or real paths `moduleA::namespaceB::foo´
	Path(PathTy)
}

pub struct RefTy {

}

pub struct SliceTy {
	inner: P<Ty>
}

pub struct ArrayTy {
	inner: P<Ty>,
	num  : usize
}

pub struct TupleTy {
	types: Vec<Ty>
}

pub struct FnTy {
	args: Vec<Ty>,
	ret_ty: P<Ty>
}

pub struct PathTy {

}
