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
	Decl(Decl),

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

pub struct Decl {
	name: Name,
	kind: DeclKind,
	visibility: Visibility
}

pub enum Visibility {
	Unspec,
	Pub,
}

pub enum DeclKind {
	/// Named binding to an expression. (`x := 42 + 1337;´)
	Expr(ExprDecl),

	/// Function definition (`add := fn(a: I32, b: I32) -> I32 { a + b }´)
	Fn(FnDecl),

	/// Struct definition (`Point2D := struct { x: F32, y: F32 }´)
	Struct(StructDecl),

	/// Enum definition (`Trite := enum { Hi, Lo, Undet })
	Enum(EnumDecl),

	/// Trait definition (`Zero := trait { zero := fn() -> Self; })
	Trait(TraitDecl),

	/// Module definition (`math := module { ... }´)
	Module(ModuleDecl)
}

pub struct ExprDecl {
	expr: P<Expr>,
	mutability: Mutability
}

pub enum Mutability {
	Immutable,
	Mutable
}

pub struct FnDecl {
	// TODO
}

pub struct StructDecl {
	// TODO
}

pub struct EnumDecl {
	// TODO
}

pub struct TraitDecl {
	// TODO
}

pub struct ModuleDecl {
	items: Vec<Item>
}
