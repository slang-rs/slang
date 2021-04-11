use crate::ast::main::AST;

use super::{
    expressions::expr_identifier,
    types::{AssignVariableStmt, BlockStmt, FunctionParam, FunctionStmt, Identifier},
};
use super::{
    expressions::{expr_access, Accessable},
    types::{ASTValue, Assignable, InitVariableStmt, TypeExpr, VarType},
};
use crate::common::position::Span;
use crate::lexer::parser::TokenType;

pub fn stmt_init_variable(ast: &mut AST) -> InitVariableStmt {
    let init_token = ast
        .check_token(
            Some(vec![TokenType::Keyword]),
            Some(vec![String::from("let"), String::from("const")]),
            true,
            true,
            0,
        )
        .unwrap();

    let constant = init_token.value == "const";

    let name = expr_identifier(ast);

    let mut type_expr: Option<TypeExpr> = None;

    if ast
        .check_one(
            TokenType::AssignmentOperator,
            String::from(":="),
            false,
            false,
        )
        .is_none()
    {
        ast.check_one(TokenType::AssignmentOperator, String::from(":"), true, true)
            .unwrap();
        type_expr = Some(ast.get_type_expr());
    }

    let mut value: Option<ASTValue> = None;

    if ast
        .check_mult(
            TokenType::AssignmentOperator,
            vec![String::from("="), String::from(":=")],
            false,
            true,
        )
        .is_some()
    {
        value = ast.get_ast_value(true, vec![], None);
    }

    InitVariableStmt {
        ntype: String::from("InitVariableStmt"),
        constant,
        name: name.clone(),
        vtype: if type_expr.is_some() {
            VarType::Static(type_expr.clone().unwrap())
        } else {
            VarType::Infer(())
        },
        value: value.clone(),
        span: Span {
            start: init_token.start,
            end: if value.is_some() {
                ASTValue::get_span(&value.unwrap()).end
            } else {
                if type_expr.is_some() {
                    type_expr.unwrap().span.end
                } else {
                    name.span.end
                }
            },
        },
    }
}

pub fn stmt_assign_variable(ast: &mut AST, up: Option<Assignable>) -> AssignVariableStmt {
    let target = if up.is_some() {
        up.unwrap()
    } else {
        Accessable::get_assignable(&expr_access(ast, false, false, true)).unwrap()
    };

    let op = ast
        .check_token(
            Some(vec![TokenType::AssignmentOperator]),
            None,
            true,
            true,
            0,
        )
        .unwrap()
        .value;

    let value = ast.get_ast_value(true, vec![], None).unwrap();

    AssignVariableStmt {
        ntype: String::from("AssignVariableStmt"),
        target: target.clone(),
        value: value.clone(),
        op,
        span: Span {
            start: target.get_span().start,
            end: value.get_span2().end,
        },
    }
}

pub fn stmt_function(ast: &mut AST, name_req: bool) -> FunctionStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("func"), true, true)
        .unwrap();

    let name: Option<Identifier>;
    if ast
        .check_token(Some(vec![TokenType::Word]), None, false, false, 0)
        .is_some()
    {
        name = Some(expr_identifier(ast));
    } else {
        if name_req {
            let err_tok = ast.get_token(0, true, true);
            ast.error(
                format!("Expected function name, but found something else"),
                if err_tok.is_some() {
                    err_tok.unwrap().start.line
                } else {
                    0
                },
                if err_tok.is_some() {
                    err_tok.unwrap().start.col
                } else {
                    0
                },
            );
        }
    }

    ast.check_one(TokenType::Parenthesis, String::from("("), true, true);

    let params: Vec<FunctionParam> = vec![];
}
