use ast::node_id::NodeId;
use ast::ptr::P;
use ast::expr::Expr;
use ast::item::Item;

pub struct Stmt {
	id  : NodeId,
	kind: StmtKind
}

pub enum StmtKind {
	Item(P<Item>),
	Expr(P<Expr>),
	Semi(P<Expr>)
}
