use super::types::{ASTValue, ArithmeticOp, BinaryOp, LogicalOp, UnaryOp};
use crate::common::position::Span;
use crate::lexer::parser::TokenType;
use crate::{ast::main::AST, common::position::Location, lexer::parser::Token};

pub fn op_arithmetic(ast: &mut AST, left: Option<ASTValue>) -> ArithmeticOp {
    let left = if left.is_some() {
        left.unwrap()
    } else {
        ast.get_ast_value(true, vec![], None).unwrap()
    };

    let op = ast
        .check_token(
            Some(vec![TokenType::ArithmeticOperator]),
            None,
            true,
            true,
            0,
        )
        .unwrap()
        .value;

    let right = ast
        .get_ast_value(
            true,
            vec![
                String::from("ArithmeticOp"),
                String::from("LogicalOp"),
                String::from("BinaryOp"),
            ],
            None,
        )
        .unwrap();

    ArithmeticOp {
        ntype: String::from("ArithmeticOp"),
        left: left.clone(),
        op,
        right: right.clone(),
        span: Span {
            start: ASTValue::get_span(&left).start,
            end: ASTValue::get_span(&right).end,
        },
    }
}

pub fn op_logical(ast: &mut AST, left: Option<ASTValue>) -> LogicalOp {
    let left = if left.is_some() {
        left.unwrap()
    } else {
        ast.get_ast_value(true, vec![], None).unwrap()
    };

    let op = ast
        .check_token(Some(vec![TokenType::LogicalOperator]), None, true, true, 0)
        .unwrap()
        .value;

    let right = ast
        .get_ast_value(
            true,
            vec![
                String::from("ArithmeticOp"),
                String::from("LogicalOp"),
                String::from("BinaryOp"),
            ],
            None,
        )
        .unwrap();

    LogicalOp {
        ntype: String::from("LogicalOp"),
        left: left.clone(),
        op,
        right: right.clone(),
        span: Span {
            start: ASTValue::get_span(&left).start,
            end: ASTValue::get_span(&right).end,
        },
    }
}

pub fn op_binary(ast: &mut AST, left: Option<ASTValue>) -> BinaryOp {
    let left = if left.is_some() {
        left.unwrap()
    } else {
        ast.get_ast_value(true, vec![], None).unwrap()
    };

    let op = ast
        .check_token(Some(vec![TokenType::BinaryOperator]), None, true, true, 0)
        .unwrap()
        .value;

    let right = ast
        .get_ast_value(
            true,
            vec![
                String::from("ArithmeticOp"),
                String::from("LogicalOp"),
                String::from("BinaryOp"),
            ],
            None,
        )
        .unwrap();

    BinaryOp {
        ntype: String::from("BinaryOp"),
        left: left.clone(),
        op,
        right: right.clone(),
        span: Span {
            start: ASTValue::get_span(&left).start,
            end: ASTValue::get_span(&right).end,
        },
    }
}

pub fn op_unary(ast: &mut AST, left: Option<ASTValue>) -> UnaryOp {
    let loc: Location;
    let op: Token;
    let value: ASTValue;

    if left.is_none()
        && ast
            .check_token(Some(vec![TokenType::UnaryOperator]), None, false, false, 0)
            .is_some()
    {
        loc = Location::Left;
        op = ast
            .check_token(Some(vec![TokenType::UnaryOperator]), None, true, true, 0)
            .unwrap();
        value = ast.get_ast_value(true, vec![], None).unwrap();
    } else {
        loc = Location::Right;
        value = ast.get_ast_value(true, vec![], None).unwrap();
        op = ast
            .check_token(Some(vec![TokenType::UnaryOperator]), None, true, true, 0)
            .unwrap();
    }

    UnaryOp {
        ntype: String::from("UnaryOp"),
        value: value.clone(),
        op: op.value,
        loc,
        span: Span {
            start: if loc == Location::Left {
                op.start
            } else {
                ASTValue::get_span(&value).start
            },
            end: if loc == Location::Right {
                op.start
            } else {
                ASTValue::get_span(&value).start
            },
        },
    }
}
