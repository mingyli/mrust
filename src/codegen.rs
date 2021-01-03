use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, PointerValue},
};

use crate::ast::*;
use crate::visit::Visitor;

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    named_values: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context) -> CodeGenerator<'ctx> {
        CodeGenerator {
            context,
            module: context.create_module("rao"),
            builder: context.create_builder(),
            named_values: HashMap::new(),
        }
    }

    pub fn finish(self) {
        self.module.print_to_file("rao.ll").unwrap();
    }
}

impl<'ctx> Visitor for CodeGenerator<'ctx> {
    type Value = Option<BasicValueEnum<'ctx>>;

    fn visit_program(&mut self, program: &Program) -> Self::Value {
        for function in &program.functions {
            self.visit_function_declaration(function);
        }
        None
    }

    fn visit_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration,
    ) -> Self::Value {
        let fn_type = if function_declaration.return_type == "i64" {
            self.context.i64_type().fn_type(&[], false)
        } else {
            self.context.void_type().fn_type(&[], false)
        };
        let fn_value = self
            .module
            .add_function(&function_declaration.name, fn_type, None);
        let block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(block);

        // The last statement is the return value.
        use itertools::{Itertools, Position};
        for statement in function_declaration.statements.iter().with_position() {
            match statement {
                Position::First(statement) | Position::Middle(statement) => {
                    self.visit_statement(statement);
                }
                Position::Last(statement) | Position::Only(statement) => {
                    let value = self.visit_statement(statement);
                    match value {
                        Some(value) => self.builder.build_return(Some(&value)),
                        None => self.builder.build_return(None),
                    };
                }
            }
        }
        // TODO: Special case? Or can be handled by compound statements?
        if function_declaration.statements.is_empty() {
            self.builder.build_return(None);
        }
        None
    }

    fn visit_statement(&mut self, statement: &Statement) -> Option<BasicValueEnum<'ctx>> {
        match statement {
            Statement::Expression(expression) => self.visit_expression(expression),
            Statement::Assignment(name, expression) => {
                let value = self.visit_expression(expression);
                if let Some(value) = value {
                    // TODO: Handle things other than ints.
                    let int_type = self.context.i64_type();
                    let pointer = self.builder.build_alloca(int_type, name);
                    self.builder.build_store(pointer, value);
                    self.named_values.insert(name.to_string(), pointer);
                }
                None
            }
        }
    }

    fn visit_expression(&mut self, expression: &Expression) -> Option<BasicValueEnum<'ctx>> {
        match expression {
            Expression::IntLiteral(value) => {
                Some(self.context.i64_type().const_int(*value, false).into())
            }
            Expression::Variable(name) => match self.named_values.get(name) {
                Some(variable) => {
                    let value = self.builder.build_load(*variable, "load");
                    Some(value)
                }
                None => unreachable!(),
            },
            Expression::Unary(operator, expression) => {
                let value = self.visit_expression(expression).unwrap().into_int_value();
                let result = match operator {
                    Operator::Minus => self.builder.build_int_neg(value, "neg"),
                    _ => unreachable!(),
                };
                Some(result.into())
            }
            Expression::Binary(operator, left, right) => {
                let left = self.visit_expression(left).unwrap().into_int_value();
                let right = self.visit_expression(right).unwrap().into_int_value();
                let result = match operator {
                    Operator::Plus => self.builder.build_int_add(left, right, "add"),
                    Operator::Minus => self.builder.build_int_sub(left, right, "sub"),
                    Operator::Times => self.builder.build_int_mul(left, right, "mul"),
                };
                Some(result.into())
            }
        }
    }
}
