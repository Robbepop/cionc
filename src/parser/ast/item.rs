use ::string_interner::{Name};
use ast::ptr::P;

pub struct Item {
	name: Name,
	kind: ItemKind
}

pub enum ItemKind {
	Module(ModuleItem),
}

pub struct ModuleItem {
	items: Vec<P<Item>>
}
