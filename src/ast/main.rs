use std::str::FromStr;

use crate::{
    common::position::{Position, Span},
    lexer::parser::{Token, TokenType},
};

use super::{
    expressions::{expr_access, expr_access_index, expr_call_function, expr_type_cast, Accessable},
    operators::{op_arithmetic, op_binary, op_logical, op_unary},
    statements::{
        stmt_assign_variable, stmt_condition, stmt_function, stmt_global_block, stmt_init_variable,
    },
    types::{ASTValue, Assignable, DotAccessable, GlobalBlockStmt, TypeExpr, TypeValue},
    value_parsers,
};

#[derive(Debug)]
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
        offset: isize,
        add_to_index: bool,
        skip_newline: bool,
    ) -> Option<Token> {
        let mut idx = (offset + (self.token_idx as isize)) as usize;
        let mut token = self.tokens.get(idx);
        if add_to_index {
            self.token_idx = ((self.token_idx as isize) + (offset + 1)) as usize;
        }
        if skip_newline {
            while token.is_some() && token.unwrap().ttype == TokenType::NewLine {
                idx += 1;
                self.token_idx += 1;
                token = self.tokens.get(idx);
            }
        }

        println!(
            "get token ({}) {} {} {} -> {:?}",
            self.token_idx, offset, add_to_index, skip_newline, token
        );

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
        offset: isize,
        skip_newline: bool,
    ) -> Option<Token> {
        let token = self.get_token(offset, false, skip_newline);
        println!("check token {} {} {:?}", error, add_to_index, token);
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

            if token.ttype == TokenType::NewLine {
                return self.check_token(ttype, value, error, true, offset + 1, skip_newline);
            }

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
                if add_to_index {
                    self.token_idx += 1;
                }

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
        self.check_token(
            Some(vec![ttype]),
            Some(vec![value]),
            error,
            add_to_index,
            0,
            false,
        )
    }

    pub fn check_mult(
        &mut self,
        ttype: TokenType,
        value: Vec<String>,
        error: bool,
        add_to_index: bool,
    ) -> Option<Token> {
        self.check_token(
            Some(vec![ttype]),
            Some(value),
            error,
            add_to_index,
            0,
            false,
        )
    }

    fn get_type(&mut self, collec: &mut Vec<TypeValue>) -> Span {
        let typev: TypeValue;
        let mut start = Position::new(0, 0);
        let mut end = Position::new(0, 0);

        let type_token = self.check_token(Some(vec![TokenType::Type]), None, false, true, 0, false);

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
                false,
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
                        false,
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
                false,
            )
            .is_some()
        {
            arr_len = Some(
                usize::from_str(
                    &self
                        .check_token(Some(vec![TokenType::Number]), None, false, true, 0, false)
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
                    false,
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
                false,
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
        let current_token = self.get_token(0, false, false);
        let mut up = up;

        if current_token.is_some() {
            let current_token = current_token.unwrap();
            if current_token.ttype == TokenType::NewLine
                || (current_token.ttype == TokenType::Operator && current_token.value == ";")
            {
                self.token_idx += 1;
                return up;
            } else if current_token.ttype == TokenType::Parenthesis && current_token.value == "(" {
                if !skip.contains(&String::from("CallFunctionExpr"))
                    && (up.is_some()
                        || up.clone().unwrap().get_type() == "CallFunctionExpr"
                        || up.clone().unwrap().get_type() == "AccessDotExpr"
                        || up.clone().unwrap().get_type() == "Identifier"
                        || up.clone().unwrap().get_type() == "AccessIndexExpr")
                {
                    up = Some(ASTValue::CallFunctionExpr(Box::new(expr_call_function(
                        self, up,
                    ))));
                } else {
                    self.check_one(TokenType::Parenthesis, String::from("("), true, true);
                    up = self.get_ast_value(true, vec![], up);
                    self.check_one(TokenType::Parenthesis, String::from(")"), true, true);
                }
            } else if !skip.contains(&"ArithmeticOp".to_string())
                && current_token.ttype == TokenType::ArithmeticOperator
            {
                up = Some(ASTValue::ArithmeticOp(Box::new(op_arithmetic(self, up))));
            } else if !skip.contains(&"AssignVariableStmt".to_string())
                && current_token.ttype == TokenType::AssignmentOperator
            {
                if up.is_some()
                    && (up.clone().unwrap().get_type() == "AccessDotExpr"
                        || up.clone().unwrap().get_type() == "Identifier"
                        || up.clone().unwrap().get_type() == "AccessIndexExpr")
                {
                    up = Some(ASTValue::AssignVariableStmt(Box::new(
                        stmt_assign_variable(
                            self,
                            Some(match up.unwrap() {
                                ASTValue::AccessDotExpr(e) => Assignable::AccessDotExpr(*e),
                                ASTValue::AccessIndexExpr(e) => Assignable::AccessIndexExpr(*e),
                                ASTValue::Identifier(e) => Assignable::Identifier(e),
                                _ => unreachable!(),
                            }),
                        ),
                    )));
                } else {
                    up = Some(ASTValue::AssignVariableStmt(Box::new(
                        stmt_assign_variable(self, None),
                    )));
                }
            } else if !skip.contains(&"LogicalOp".to_string())
                && current_token.ttype == TokenType::LogicalOperator
            {
                up = Some(ASTValue::LogicalOp(Box::new(op_logical(self, up))));
            } else if !skip.contains(&"BinaryOp".to_string())
                && current_token.ttype == TokenType::BinaryOperator
            {
                up = Some(ASTValue::BinaryOp(Box::new(op_binary(self, up))));
            } else if !skip.contains(&"UnaryOp".to_string())
                && current_token.ttype == TokenType::UnaryOperator
            {
                up = Some(ASTValue::UnaryOp(Box::new(op_unary(self, up))));
            } else if (!skip.contains(&"AccessDotExpr".to_string())
                || !skip.contains(&"Identifier".to_string())
                || !skip.contains(&"CallFunctionExpr".to_string()))
                && current_token.ttype == TokenType::Word
            {
                println!("astv expr access {:?}", current_token);
                let res = expr_access(self, false, false, false);
                let astv = if res.is_some() {
                    Some(Accessable::get_ast_value(&res.unwrap()))
                } else {
                    None
                };
                if astv.is_some() && !skip.contains(&astv.clone().unwrap().get_type()) {
                    up = Some(astv.unwrap());
                } else {
                    if error {
                        self.error(
                            format!("Unexpected token {}", current_token.value),
                            current_token.start.line,
                            current_token.start.col,
                        );
                    } else {
                        return up;
                    }
                }
            } else if !skip.contains(&"StringParsed".to_string())
                && current_token.ttype == TokenType::String
                && up.is_none()
            {
                up = Some(ASTValue::StringParsed(value_parsers::value_string_parser(
                    self,
                )));
                return up; // idk
            } else if !skip.contains(&"IntParsed".to_string())
                && current_token.ttype == TokenType::Number
                && up.is_none()
            {
                up = Some(ASTValue::IntParsed(value_parsers::value_int_parser(self)));
                // return up; // idk
            } else if !skip.contains(&"FloatParsed".to_string())
                && current_token.ttype == TokenType::Float
                && up.is_none()
            {
                up = Some(ASTValue::FloatParsed(value_parsers::value_float_parser(
                    self,
                )));
                return up; // idk
            } else if (!skip.contains(&"AccessIndexExpr".to_string())
                || !skip.contains(&"ArrayParsed".to_string()))
                && current_token.ttype == TokenType::SqBraces
                && current_token.value == "["
            {
                if !skip.contains(&"AccessIndexExpr".to_string())
                    && (up.is_some()
                        && (up.clone().unwrap().get_type() == "AccessDotExpr"
                            || up.clone().unwrap().get_type() == "Identifier"
                            || up.clone().unwrap().get_type() == "CallFunctionExpr"))
                {
                    up = Some(ASTValue::AccessIndexExpr(Box::new(expr_access_index(
                        self,
                        Some(match up.unwrap() {
                            ASTValue::CallFunctionExpr(e) => DotAccessable::CallFunctionExpr(*e),
                            ASTValue::AccessIndexExpr(e) => DotAccessable::AccessIndexExpr(e),
                            ASTValue::AccessDotExpr(e) => DotAccessable::AccessDotExpr(e),
                            ASTValue::Identifier(e) => DotAccessable::Identifier(e),
                            _ => unreachable!(),
                        }),
                    ))));
                } else if !skip.contains(&"ArrayParsed".to_string()) && up.is_none() {
                    up = Some(ASTValue::ArrayParsed(value_parsers::value_array_parser(
                        self,
                    )));
                }
            } else if !skip.contains(&"DictParsed".to_string())
                && current_token.ttype == TokenType::Braces
                && current_token.value == "{"
                && up.is_none()
            {
                up = Some(ASTValue::DictParsed(value_parsers::value_dict_parser(self)));
            } else if (!skip.contains(&"BoolParsed".to_string())
                || !skip.contains(&"FunctionStmt".to_string())
                || !skip.contains(&"TypeCastExpr".to_string()))
                && current_token.ttype == TokenType::Keyword
            {
                if !skip.contains(&"BoolParsed".to_string())
                    && (current_token.value == "true" || current_token.value == "false")
                    && up.is_none()
                {
                    up = Some(ASTValue::BoolParsed(value_parsers::value_bool_parser(self)));
                } else if !skip.contains(&"FunctionStmt".to_string())
                    && current_token.value == "func"
                    && up.is_none()
                {
                    up = Some(ASTValue::FunctionStmt(Box::new(stmt_function(
                        self, true, None,
                    ))));
                    return up;
                } else if !skip.contains(&"ConditionStmt".to_string())
                    && current_token.value == "if"
                    && up.is_none()
                {
                    up = Some(ASTValue::ConditionStmt(Box::new(stmt_condition(self))));
                } else if !skip.contains(&"InitVariableStmt".to_string())
                    && (current_token.value == "let" || current_token.value == "const")
                    && up.is_none()
                {
                    up = Some(ASTValue::InitVariableStmt(Box::new(stmt_init_variable(
                        self,
                    ))));
                } else if !skip.contains(&"TypeCastExpr".to_string()) && current_token.value == "as"
                {
                    up = Some(ASTValue::TypeCastExpr(Box::new(expr_type_cast(self, up))));
                } else {
                    if error {
                        self.error(
                            format!("Unexpected token {}", current_token.value),
                            current_token.start.line,
                            current_token.start.col,
                        );
                    } else {
                        return up;
                    }
                }
            } else if !skip.contains(&"NullParsed".to_string())
                && current_token.ttype == TokenType::Type
                && current_token.value == "null"
            {
                up = Some(ASTValue::NullParsed(value_parsers::value_null_parser(self)));
            } else {
                if error {
                    self.error(
                        format!("Unexpected syntax {}", current_token.value),
                        current_token.start.line,
                        current_token.start.col,
                    );
                }
                return up;
            }

            let test_token = self.get_token(-1, false, false);
            if test_token.is_some()
                && (test_token.clone().unwrap().ttype == TokenType::NewLine
                    || (test_token.clone().unwrap().ttype == TokenType::Operator
                        && test_token.clone().unwrap().value == ";"))
            {
                up
            } else {
                println!("fallback test {:?}", up);
                self.get_ast_value(false, skip, up)
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
            if up.is_some() {
                up
            } else {
                None
            }
        }
    }

    pub fn get_global_block(&mut self) -> GlobalBlockStmt {
        stmt_global_block(self)
    }
}
