use ast::node_id::NodeId;
use ast::ptr::P;
use ast::expr::Expr;

use ::string_interner::{Name};

pub struct Item {
	id  : NodeId,
	kind: ItemKind
}

pub enum ItemKind {
	/// Named items (`foo := struct {...}´) are Declarations
	Defn(Defn),

	/// Use item (`use foo::bar::baz;´)
	Use(UseItem),

	/// Impl blocks (`impl Foo for Bar {...}´)
	Impl(ImplItem)
}

pub struct UseItem {
	// TODO
}

pub struct ImplItem {
	// TODO
}

pub struct Defn {
	name: Name,
	kind: DefnKind,
	visibility: Visibility
}

pub enum Visibility {
	Unspec,
	Pub,
}

pub enum DefnKind {
	/// Named binding to an expression. (`x := 42 + 1337;´)
	Expr(ExprDefn),

	/// Function definition (`add := fn(a: I32, b: I32) -> I32 { a + b }´)
	Fn(FnDefn),

	/// Struct definition (`Point2D := struct { x: F32, y: F32 }´)
	Struct(StructDefn),

	/// Enum definition (`Trite := enum { Hi, Lo, Undet })
	Enum(EnumDefn),

	/// Trait definition (`Zero := trait { zero := fn() -> Self; })
	Trait(TraitDefn),

	/// Module definition (`math := module { ... }´)
	Module(ModuleDefn)
}

pub struct ExprDefn {
	expr: P<Expr>,
	mutability: Mutability
}

pub enum Mutability {
	Immutable,
	Mutable
}

pub struct FnDefn {
	// TODO
}

pub struct StructDefn {
	// TODO
}

pub struct EnumDefn {
	// TODO
}

pub struct TraitDefn {
	// TODO
}

pub struct ModuleDefn {
	items: Vec<Item>
}
