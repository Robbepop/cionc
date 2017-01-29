use ast::node_id::NodeId;
use ast::ptr::P;
use ast::expr::Expr;

use ::string_interner::{Name};

pub struct Item {
	id  : NodeId,
	kind: ItemKind
}

pub enum ItemKind {
	Decl(Decl),
	Use(UseItem),
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
	Expr(ExprDecl),
	Fn(FnDecl),
	Struct(StructDecl),
	Enum(EnumDecl),
	Trait(TraitDecl),
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
