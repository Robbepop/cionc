
struct Item {
	name: Name,
	kind: ItemKind
}

enum ItemKind {
	Module(P<ModuleItem>),
}

struct ModuleItem;
