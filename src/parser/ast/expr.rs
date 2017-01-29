use ast::node_id::NodeId;
use ast::ptr::P;
use ast::stmt::Stmt;

pub struct Expr {
	id:   NodeId,
	kind: ExprKind
}

pub enum ExprKind {
	Block(BlockExpr),

	Lit(LitExpr),
	Tuple(TupleExpr),
	Array(ArrayExpr),
	Range(RangeExpr),

	Call(CallExpr),
	Index(IndexExpr),

	Binary(BinaryExpr),
	Unary(UnaryExpr),
	If(IfExpr),
	Match(MatchExpr),

	Assign(AssignExpr),
	AssignOp(AssignOpExpr)
}

pub struct BlockExpr {
    stmts: Vec<Stmt>
}

pub struct LitExpr {

}

pub struct TupleExpr {
	vals: Vec<Expr>
}

pub struct ArrayExpr {
	elems: Vec<Expr>
}

pub struct RangeExpr {
	from: Option<P<Expr>>,
	to: Option<P<Expr>>
}

pub struct CallExpr {
	expr: P<Expr>,
	args: Vec<Expr>
}

pub struct IndexExpr {
	expr: P<Expr>,
	args: Vec<Expr>
}

pub struct BinaryExpr {
	lhs: P<Expr>,
	rhs: P<Expr>,
	kind: BinaryKind
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinaryKind {
    Plus,        // +
    Minus,       // -
    Star,        // *
    Slash,       // /
    Percent,     // %
    Caret,       // ^

    Amp,         // &
    Pipe,        // |
    Shl,         // <<
    Shr,         // >>

    AmpAmp,      // &&
    PipePipe,    // ||

    EqEq,        // ==
    NotEq,       // !=
    LessThan,    // <
    LessEq,      // =<
    GreaterThan, // >
    GreaterEq    // >=
}

pub struct UnaryExpr {
	expr: P<Expr>,
	kind: UnaryKind
}

pub enum UnaryKind {
	Minus,
	Not,
	Star,
	Amp
}

pub struct IfExpr {
	cond: P<Expr>,
	then_expr: P<Expr>,
	else_expr: Option<P<Expr>>
}

pub struct MatchExpr {
	matcher: P<Expr>
	// datastructure for the match arms
}

pub struct AssignExpr {
	lhs: P<Expr>,
	rhs: P<Expr>
}

pub struct AssignOpExpr {
	lhs: P<Expr>,
	rhs: P<Expr>,
	op: BinaryKind
}
