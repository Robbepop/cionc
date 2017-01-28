
struct Expr {
	kind: ExprKind
}

enum ExprKind {
	Const(P<ConstExpr>),
	Call(P<CallExpr>),
	BinOp(P<BinOpExpr>),
	UnOp(P<UnOpExpr>),
	If(P<IfExpr>),
	Match(P<MatchExpr>),
}

struct ConstExpr;
struct CallExpr;
struct BinOpExpr;
struct UnOpExpr;
struct IfExpr;
struct MatchExpr;
