use bevy_bsn_ast::syn::{self, visit::Visit, Macro};

/// Visits and collects all `bsn!` macro invocations in an File AST.
pub fn visit_and_collect_macros(ast: &syn::File) -> Vec<&Macro> {
    let mut visitor = BsnMacroVisitor::default();
    visitor.visit_file(ast);
    visitor.invocations
}

#[derive(Default)]
struct BsnMacroVisitor<'ast> {
    invocations: Vec<&'ast Macro>,
}

impl<'ast> Visit<'ast> for BsnMacroVisitor<'ast> {
    fn visit_macro(&mut self, node: &'ast Macro) {
        if node.path.is_ident("bsn") {
            self.invocations.push(node);
        }
        syn::visit::visit_macro(self, node);
    }
}
