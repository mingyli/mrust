use crate::ast::*;

pub trait Visitor {
    type Value;

    fn visit_program(&mut self, program: &Program) -> Self::Value;
    fn visit_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration,
    ) -> Self::Value;
    fn visit_statement(&mut self, statement: &Statement) -> Self::Value;
    fn visit_expression(&mut self, expression: &Expression) -> Self::Value;
}
