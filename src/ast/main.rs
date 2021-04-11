use std::str::FromStr;

use crate::{
    common::position::{Position, Span},
    lexer::parser::{Token, TokenType},
};

use super::types::{ASTValue, TypeExpr, TypeValue};

pub struct ASTError {
    pub msg: String,
    pub pos: Position,
}

pub struct AST {
    pub errors: Vec<ASTError>,
    pub tokens: Vec<Token>,
    pub token_idx: usize,
}

impl AST {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            errors: vec![],
            tokens,
            token_idx: 0,
        }
    }

    pub fn error(&mut self, msg: String, line: u32, col: u32) {
        self.errors.push(ASTError {
            msg,
            pos: Position::new(line, col),
        });
    }

    pub fn reset(&mut self) {
        self.errors.clear();
        self.token_idx = 0;
    }

    pub fn get_token(
        &mut self,
        offset: usize,
        add_to_index: bool,
        skip_newline: bool,
    ) -> Option<Token> {
        let mut idx = offset + self.token_idx;
        let mut token = self.tokens.get(idx);
        if add_to_index {
            self.token_idx += offset + 1;
        }
        if skip_newline {
            while token.is_some() && token.unwrap().ttype == TokenType::NewLine {
                idx += 1;
                self.token_idx += 1;
                token = self.tokens.get(idx);
            }
        }

        if token.is_some() {
            Some(token.unwrap().clone())
        } else {
            None
        }
    }

    pub fn check_token(
        &mut self,
        ttype: Option<Vec<TokenType>>,
        value: Option<Vec<String>>,
        error: bool,
        add_to_index: bool,
        offset: usize,
    ) -> Option<Token> {
        let token = self.get_token(offset, add_to_index, true);
        if token.is_none() {
            if error {
                let last_token = self.tokens.last();
                let line = if last_token.is_some() {
                    last_token.unwrap().start.line
                } else {
                    0
                };
                let col = if last_token.is_some() {
                    last_token.unwrap().start.col
                } else {
                    0
                };
                self.error(String::from("Expected Token, but found EOF"), line, col)
            }

            None
        } else {
            let token = token.unwrap();

            let mut match_ttype = true;
            if ttype.is_some() {
                let ttype = ttype.unwrap();
                if !ttype.contains(&token.ttype) {
                    match_ttype = false;
                }
            }

            let mut match_value = true;
            if value.is_some() {
                let value = value.unwrap();
                if !value.contains(&token.value) {
                    match_value = false;
                }
            }

            if match_ttype && match_value {
                Some(token)
            } else {
                if error {
                    self.error(
                        format!("Unexpected token {} ({:?})", token.value, token.ttype),
                        token.start.line,
                        token.start.col,
                    )
                }

                None
            }
        }
    }

    pub fn check_one(
        &mut self,
        ttype: TokenType,
        value: String,
        error: bool,
        add_to_index: bool,
    ) -> Option<Token> {
        self.check_token(Some(vec![ttype]), Some(vec![value]), error, add_to_index, 0)
    }

    pub fn check_mult(
        &mut self,
        ttype: TokenType,
        value: Vec<String>,
        error: bool,
        add_to_index: bool,
    ) -> Option<Token> {
        self.check_token(Some(vec![ttype]), Some(value), error, add_to_index, 0)
    }

    fn get_type(&mut self, collec: &mut Vec<TypeValue>) -> Span {
        let typev: TypeValue;
        let mut start = Position::new(0, 0);
        let mut end = Position::new(0, 0);

        let type_token = self.check_token(Some(vec![TokenType::Type]), None, false, true, 0);

        if type_token.is_some() {
            let type_token = type_token.unwrap();
            typev = TypeValue::Static(type_token.value);
            start = type_token.start;
            end = type_token.end;
        } else {
            let with_paren = self.check_token(
                Some(vec![TokenType::Parenthesis]),
                Some(vec![String::from("(")]),
                false,
                true,
                0,
            );

            if with_paren.is_some() {
                let with_paren = with_paren.unwrap();
                let expr: TypeExpr = self.get_type_expr();
                typev = TypeValue::Expr(expr);
                start = with_paren.start;
                end = self
                    .check_token(
                        Some(vec![TokenType::Parenthesis]),
                        Some(vec![String::from(")")]),
                        true,
                        true,
                        0,
                    )
                    .unwrap()
                    .end;
            } else {
                let astv = self
                    .get_ast_value(
                        true,
                        vec![
                            "AssignVariableStmt".to_string(),
                            "ArithmeticOp".to_string(),
                            "BinaryOp".to_string(),
                            "UnaryOp".to_string(),
                            "LogicalOp".to_string(),
                            "ArrayParsed".to_string(),
                            "AccessIndexExpr".to_string(),
                        ],
                        None,
                    )
                    .unwrap();
                typev = TypeValue::Dynamic(astv);
            }
        }

        let arr_len: Option<usize>;
        if self
            .check_token(
                Some(vec![TokenType::SqBraces]),
                Some(vec![String::from("[")]),
                false,
                true,
                0,
            )
            .is_some()
        {
            arr_len = Some(
                usize::from_str(
                    &self
                        .check_token(Some(vec![TokenType::Number]), None, false, true, 0)
                        .unwrap()
                        .value,
                )
                .unwrap(),
            );

            end = self
                .check_token(
                    Some(vec![TokenType::SqBraces]),
                    Some(vec![String::from("]")]),
                    true,
                    true,
                    0,
                )
                .unwrap()
                .end;

            collec.push(TypeValue::Expr(TypeExpr {
                span: Span { start, end },
                ntype: "TypeExpr".to_string(),
                value: vec![typev],
                arr_len,
            }));
        }

        Span { start, end }
    }

    pub fn get_type_expr(&mut self) -> TypeExpr {
        let mut types = Vec::<TypeValue>::new();

        let mut span = self.get_type(&mut types);
        while self
            .check_token(
                Some(vec![TokenType::BinaryOperator]),
                Some(vec![String::from("|")]),
                false,
                true,
                0,
            )
            .is_some()
        {
            span.end = self.get_type(&mut types).end;
        }

        TypeExpr {
            ntype: String::from("TypeExpr"),
            value: types,
            span,
            arr_len: None,
        }
    }

    pub fn get_ast_value(
        &mut self,
        error: bool,
        skip: Vec<String>,
        up: Option<ASTValue>,
    ) -> Option<ASTValue> {
        let current_token = self.get_token(0, true, false);

        if current_token.is_some() {
            let current_token = current_token.unwrap();
            if current_token.ttype == TokenType::NewLine
                || (current_token.ttype == TokenType::Operator && current_token.value == ";")
            {
                self.token_idx += 1;
                up
            } else {
                None
            }
        } else {
            if error {
                self.error(
                    format!("Expected Token, but got EOF instead"),
                    if self.tokens.len() != 0 {
                        self.tokens.last().unwrap().start.line
                    } else {
                        0
                    },
                    if self.tokens.len() != 0 {
                        self.tokens.last().unwrap().start.col
                    } else {
                        0
                    },
                )
            }
            None
        }
    }
}
