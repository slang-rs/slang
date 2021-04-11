use crate::ast::main::AST;
use crate::common::position::Span;
use crate::lexer::parser::TokenType;
use std::str::FromStr;

use super::types::{
    ASTValue, ArrayParsed, BoolParsed, DictElement, DictParsed, FloatParsed, FloatType, FloatValue,
    IntParsed, IntValue, NullParsed, NumType, StringParsed,
};

use super::expressions::expr_identifier;

pub fn value_string_parser(ast: &mut AST) -> StringParsed {
    let token = ast
        .check_token(Some(vec![TokenType::String]), None, true, true, 0)
        .unwrap();

    StringParsed {
        ntype: String::from("StringParsed"),
        value: token.value,
        span: Span {
            start: token.start,
            end: token.end,
        },
    }
}

pub fn value_int_parser(ast: &mut AST, ntype: NumType) -> IntParsed {
    let token = ast
        .check_token(Some(vec![TokenType::Number]), None, true, true, 0)
        .unwrap();

    let value = match ntype {
        NumType::U8 => IntValue::U8(u8::from_str(&token.value).unwrap()),
        NumType::U16 => IntValue::U16(u16::from_str(&token.value).unwrap()),
        NumType::U32 => IntValue::U32(u32::from_str(&token.value).unwrap()),
        NumType::U64 => IntValue::U64(u64::from_str(&token.value).unwrap()),
        NumType::I8 => IntValue::I8(i8::from_str(&token.value).unwrap()),
        NumType::I16 => IntValue::I16(i16::from_str(&token.value).unwrap()),
        NumType::I32 => IntValue::I32(i32::from_str(&token.value).unwrap()),
        NumType::I64 => IntValue::I64(i64::from_str(&token.value).unwrap()),
    };

    IntParsed {
        ntype: String::from("IntParsed"),
        value,
        span: Span {
            start: token.start,
            end: token.end,
        },
    }
}

pub fn value_float_parser(ast: &mut AST, ntype: FloatType) -> FloatParsed {
    let token = ast
        .check_token(Some(vec![TokenType::Number]), None, true, true, 0)
        .unwrap();

    let value = match ntype {
        FloatType::F32 => FloatValue::F32(f32::from_str(&token.value).unwrap()),
        FloatType::F64 => FloatValue::F64(f64::from_str(&token.value).unwrap()),
    };

    FloatParsed {
        ntype: String::from("FloatParsed"),
        value,
        span: Span {
            start: token.start,
            end: token.end,
        },
    }
}

pub fn value_bool_parser(ast: &mut AST) -> BoolParsed {
    let token = ast
        .check_token(
            Some(vec![TokenType::String]),
            Some(vec![String::from("true"), String::from("false")]),
            true,
            true,
            0,
        )
        .unwrap();

    BoolParsed {
        ntype: String::from("BoolParsed"),
        value: if token.value == "true" { true } else { false },
        span: Span {
            start: token.start,
            end: token.end,
        },
    }
}

pub fn value_null_parser(ast: &mut AST) -> NullParsed {
    let token = ast
        .check_token(
            Some(vec![TokenType::String]),
            Some(vec![String::from("null")]),
            true,
            true,
            0,
        )
        .unwrap();

    NullParsed {
        ntype: String::from("NullParsed"),
        span: Span {
            start: token.start,
            end: token.end,
        },
    }
}

pub fn value_array_parser(ast: &mut AST) -> ArrayParsed {
    let token_start = ast
        .check_token(
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("[")]),
            true,
            true,
            0,
        )
        .unwrap();

    let mut elements = Vec::<ASTValue>::new();

    while ast
        .check_token(
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("]")]),
            false,
            false,
            0,
        )
        .is_none()
    {
        elements.push(ast.get_ast_value(true, vec![], None).unwrap());
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
            Some(vec![TokenType::SqBraces]),
            Some(vec![String::from("]")]),
            true,
            true,
            0,
        )
        .unwrap();

    ArrayParsed {
        ntype: String::from("ArrayParsed"),
        elements,
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
    }
}

pub fn value_dict_parser(ast: &mut AST) -> DictParsed {
    let token_start = ast
        .check_token(
            Some(vec![TokenType::Braces]),
            Some(vec![String::from("{")]),
            true,
            true,
            0,
        )
        .unwrap();

    let mut entries = Vec::<DictElement>::new();

    while ast
        .check_token(
            Some(vec![TokenType::Braces]),
            Some(vec![String::from("}")]),
            false,
            false,
            0,
        )
        .is_none()
    {
        let el_name = expr_identifier(ast);

        ast.check_token(
            Some(vec![TokenType::Operator]),
            Some(vec![String::from(":")]),
            true,
            true,
            0,
        );

        let el_value = ast.get_ast_value(true, vec![], None).unwrap();

        entries.push(DictElement {
            span: Span {
                start: el_name.span.start,
                end: ASTValue::get_span(&el_value).end,
            },
            key: el_name,
            value: el_value,
        });

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
            Some(vec![TokenType::Braces]),
            Some(vec![String::from("}")]),
            true,
            true,
            0,
        )
        .unwrap();

    DictParsed {
        ntype: String::from("ArrayParsed"),
        entries,
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
    }
}
