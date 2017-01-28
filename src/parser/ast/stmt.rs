
struct Stmt {
	kind: StmtKind
}

enum StmtKind {
	Let(P<LetStmt>),
	Expr(P<Expr>),
	Semi(P<Expr>)
}
