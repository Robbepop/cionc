use ast::ptr::P;
use ast::expr::Expr;
use ast::item::Item;

pub struct Stmt {
	kind: StmtKind
}

pub enum StmtKind {
	Let(LetStmt),
	Item(P<Item>),
	Expr(P<Expr>),
	Semi(P<Expr>)
}

pub struct LetStmt {

}
