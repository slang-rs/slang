use crate::ast::main::AST;

use super::types::{
    ASTValue, AccessDotExpr, AccessIndexExpr, Assignable, CallFunctionExpr, DotAccessable,
    Identifier, TypeCastExpr,
};
use crate::common::position::Span;
use crate::lexer::parser::TokenType;

pub fn expr_identifier(ast: &mut AST) -> Identifier {
    let token = ast
        .check_token(Some(vec![TokenType::Word]), None, true, true, 0)
        .unwrap();

    Identifier {
        ntype: String::from("Identifier"),
        span: Span {
            start: token.start,
            end: token.end,
        },
        name: token.value,
    }
}

pub fn expr_access_dot(ast: &mut AST, left: Option<DotAccessable>) -> AccessDotExpr {
    println!("expr access dot");
    let left = if left.is_some() {
        left.unwrap()
    } else {
        Accessable::get_dot_access(&expr_access(ast, true, true, false).unwrap())
    };

    let mut return_null = false;

    if ast
        .check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from("?.")]),
            false,
            true,
            0,
        )
        .is_some()
    {
        return_null = true;
    } else {
        ast.check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from(".")]),
            true,
            true,
            0,
        )
        .unwrap();
    }

    let right = expr_access(ast, false, false, false);
    let right = Accessable::get_dot_access(&right.unwrap());
    let left_clone = left.clone();
    let right_clone = right.clone();

    AccessDotExpr {
        ntype: String::from("AccessDotExpr"),
        left,
        return_null,
        right,
        span: Span {
            start: DotAccessable::get_span(&left_clone).start,
            end: DotAccessable::get_span(&right_clone).end,
        },
    }
}

pub fn expr_access_index(ast: &mut AST, left: Option<DotAccessable>) -> AccessIndexExpr {
    let left = if left.is_some() {
        left.unwrap()
    } else {
        Accessable::get_dot_access(&expr_access(ast, false, true, false).unwrap())
    };

    let token_start = ast
        .check_token(
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("[")]),
            true,
            true,
            0,
        )
        .unwrap();

    let right = ast.get_ast_value(true, vec![], None).unwrap();

    let token_end = ast
        .check_token(
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("[")]),
            true,
            true,
            0,
        )
        .unwrap();

    AccessIndexExpr {
        ntype: String::from("AccessIndexExpr"),
        left,
        right,
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
    }
}

pub fn expr_call_function(ast: &mut AST, left: Option<ASTValue>) -> CallFunctionExpr {
    println!("expr call fn");
    let left = if left.is_some() {
        left.unwrap()
    } else {
        Accessable::get_ast_value(&expr_access(ast, false, false, true).unwrap())
    };

    let token_start = ast
        .check_token(
            Some(vec![TokenType::Parenthesis]),
            Some(vec![String::from("(")]),
            true,
            true,
            0,
        )
        .unwrap();

    let mut params: Vec<ASTValue> = vec![];

    while ast
        .check_token(
            Some(vec![TokenType::Parenthesis]),
            Some(vec![String::from(")")]),
            false,
            false,
            0,
        )
        .is_none()
    {
        params.push(ast.get_ast_value(true, vec![], None).unwrap());

        ast.check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from(",")]),
            false,
            true,
            0,
        );
    }

    let token_end = ast
        .check_token(
            Some(vec![TokenType::Parenthesis]),
            Some(vec![String::from(")")]),
            true,
            true,
            0,
        )
        .unwrap();

    CallFunctionExpr {
        ntype: String::from("CallFunctionExpr"),
        what: left,
        params,
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
    }
}

pub enum Accessable {
    AccessDotExpr(AccessDotExpr),
    AccessIndexExpr(AccessIndexExpr),
    Identifier(Identifier),
    CallFunctionExpr(CallFunctionExpr),
}

impl Accessable {
    pub fn get_ast_value(val: &Accessable) -> ASTValue {
        match val {
            Accessable::AccessDotExpr(e) => ASTValue::AccessDotExpr(Box::new(e.clone())),
            Accessable::AccessIndexExpr(e) => ASTValue::AccessIndexExpr(Box::new(e.clone())),
            Accessable::Identifier(e) => ASTValue::Identifier(e.clone()),
            Accessable::CallFunctionExpr(e) => ASTValue::CallFunctionExpr(Box::new(e.clone())),
        }
    }

    pub fn get_dot_access(val: &Accessable) -> DotAccessable {
        match val {
            Accessable::AccessDotExpr(e) => DotAccessable::AccessDotExpr(Box::new(e.clone())),
            Accessable::AccessIndexExpr(e) => DotAccessable::AccessIndexExpr(Box::new(e.clone())),
            Accessable::Identifier(e) => DotAccessable::Identifier(e.clone()),
            Accessable::CallFunctionExpr(e) => DotAccessable::CallFunctionExpr(e.clone()),
        }
    }

    pub fn get_assignable(val: &Accessable) -> Option<Assignable> {
        match val {
            Accessable::AccessDotExpr(e) => Some(Assignable::AccessDotExpr(e.clone())),
            Accessable::AccessIndexExpr(e) => Some(Assignable::AccessIndexExpr(e.clone())),
            Accessable::Identifier(e) => Some(Assignable::Identifier(e.clone())),
            _ => None,
        }
    }
}

pub fn expr_access(
    ast: &mut AST,
    skip_dot: bool,
    skip_index: bool,
    skip_func: bool,
) -> Option<Accessable> {
    println!("expr access {} {} {}", skip_dot, skip_index, skip_func);
    ast.check_token(Some(vec![TokenType::Word]), None, false, false, 0);
    println!("expr check");

    let is_dot = ast
        .check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from("?."), String::from(".")]),
            false,
            false,
            1,
        )
        .is_some();

    let is_func = ast
        .check_token(
            Some(vec![TokenType::Parenthesis]),
            Some(vec![String::from("(")]),
            false,
            false,
            1,
        )
        .is_some();

    let is_index = ast
        .check_token(
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("[")]),
            false,
            false,
            0,
        )
        .is_some();

    if !skip_dot && is_dot {
        println!("is dot");
        Some(Accessable::AccessDotExpr(expr_access_dot(ast, None)))
    } else if !skip_func && is_func {
        println!("is func call");
        let result = expr_call_function(ast, None);

        if ast
            .check_token(
                Some(vec![TokenType::Operator]),
                Some(vec![String::from("."), String::from("?.")]),
                false,
                false,
                0,
            )
            .is_some()
        {
            Some(Accessable::AccessDotExpr(expr_access_dot(ast, None)))
        } else {
            Some(Accessable::CallFunctionExpr(result))
        }
    } else if !skip_index && is_index {
        println!("is index");
        Some(Accessable::AccessIndexExpr(expr_access_index(ast, None)))
    } else if ast
        .check_token(Some(vec![TokenType::Word]), None, false, false, 0)
        .is_some()
    {
        println!("is ident");
        Some(Accessable::Identifier(expr_identifier(ast)))
    } else {
        None
    }
}

pub fn expr_type_cast(ast: &mut AST, up: Option<ASTValue>) -> TypeCastExpr {
    let value = if up.is_some() {
        up.unwrap()
    } else {
        ast.get_ast_value(true, vec![], None).unwrap()
    };

    ast.check_token(
        Some(vec![TokenType::Keyword]),
        Some(vec![String::from("as")]),
        true,
        true,
        0,
    )
    .unwrap();

    let return_null = ast
        .check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from("?")]),
            true,
            true,
            0,
        )
        .is_some();

    let target_type = ast.get_type_expr();
    let target_type_clone = target_type.clone();
    let value_clone = value.clone();

    TypeCastExpr {
        ntype: String::from("TypeCastExpr"),
        value,
        target_type,
        return_null,
        span: Span {
            start: ASTValue::get_span(&value_clone).start,
            end: target_type_clone.span.end,
        },
    }
}
