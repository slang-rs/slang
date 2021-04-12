use crate::ast::main::AST;

use super::{
    expressions::expr_identifier,
    types::{
        ASTNode, AssignVariableStmt, BlockStmt, ClassStmt, ConditionStmt, Extendable, ForStmt,
        FunctionParam, FunctionStmt, Identifier, InterfaceMember, InterfaceStmt, ReturnStmt,
        WhileStmt,
    },
};
use super::{
    expressions::{expr_access, Accessable},
    types::{ASTValue, Assignable, InitVariableStmt, TypeExpr, VarType},
};
use crate::common::position::{Position, Span};
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

pub fn stmt_function(ast: &mut AST, name_req: bool, word: Option<String>) -> FunctionStmt {
    let token_start = ast
        .check_one(
            if word.is_some() {
                TokenType::Word
            } else {
                TokenType::Keyword
            },
            if word.is_some() {
                word.clone().unwrap()
            } else {
                String::from("func")
            },
            true,
            true,
        )
        .unwrap();

    let mut name: Option<Identifier> = None;
    if word.is_none()
        && ast
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
                    err_tok.clone().unwrap().start.line
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

    let mut params: Vec<FunctionParam> = vec![];
    while ast
        .check_one(TokenType::Parenthesis, String::from(")"), false, true)
        .is_none()
    {
        let name = expr_identifier(ast);

        ast.check_one(TokenType::Operator, String::from(":"), true, true);
        let ptype = ast.get_type_expr();

        let mut default: Option<ASTValue> = None;
        if ast
            .check_one(
                TokenType::AssignmentOperator,
                String::from("="),
                false,
                true,
            )
            .is_some()
        {
            default = ast.get_ast_value(true, vec![], None);
        }

        params.push(FunctionParam {
            name: name.clone(),
            span: Span {
                start: name.span.start,
                end: if default.is_some() {
                    default.clone().unwrap().get_span2().end
                } else {
                    ptype.span.end
                },
            },
            ptype: VarType::Static(ptype),
            default,
        })
    }

    let rtype;
    if ast
        .check_one(TokenType::Operator, String::from(":"), false, true)
        .is_some()
    {
        rtype = VarType::Static(ast.get_type_expr());
    } else {
        rtype = VarType::Infer(());
    }

    let block = stmt_block(ast, false);

    FunctionStmt {
        ntype: String::from("FunctionStmt"),
        span: Span {
            start: token_start.start,
            end: block.span.end,
        },
        block,
        name,
        rtype,
        params,
    }
}

pub fn stmt_return(ast: &mut AST) -> ReturnStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("return"), true, true)
        .unwrap();
    let value = ast.get_ast_value(false, vec![], None);

    ReturnStmt {
        ntype: String::from("ReturnStmt"),
        span: Span {
            start: token_start.start,
            end: if value.is_some() {
                value.clone().unwrap().get_span2().end
            } else {
                token_start.end
            },
        },
        value,
    }
}

pub fn stmt_block(ast: &mut AST, global: bool) -> BlockStmt {
    let mut body: Vec<ASTNode> = vec![];
    let mut start = Position::new(0, 0);

    if !global {
        start = ast
            .check_one(TokenType::Braces, String::from("{"), true, true)
            .unwrap()
            .start;
    }

    let mut token = ast.get_token(0, false, true);
    while token.is_some()
        && token.clone().unwrap().ttype != TokenType::Braces
        && token.clone().unwrap().value != "}"
    {
        let tok = token.unwrap();
        if tok.ttype == TokenType::Keyword {
            if tok.value == "let" || tok.value == "const" {
                body.push(ASTNode::InitVariableStmt(stmt_init_variable(ast)));
            } else if tok.value == "return" {
                body.push(ASTNode::ReturnStmt(stmt_return(ast)));
            }
        }

        token = ast.get_token(0, false, true);
    }

    let mut end: Option<Position> = None;
    if token.is_some() {
        end = Some(token.unwrap().end);
    }

    if !global {
        end = Some(
            ast.check_one(TokenType::Braces, String::from("}"), true, true)
                .unwrap()
                .end,
        );
    }

    if end.is_none() {
        let prev_tok = ast.get_token(0, false, true);
        end = Some(if prev_tok.is_some() {
            prev_tok.unwrap().end
        } else {
            Position::new(0, 0)
        });
    }

    BlockStmt {
        ntype: String::from("BlockStmt"),
        span: Span {
            start,
            end: end.unwrap(),
        },
        body,
    }
}

pub fn stmt_interface(ast: &mut AST) -> InterfaceStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("interface"), true, true)
        .unwrap();

    let name = expr_identifier(ast);
    let mut members: Vec<InterfaceMember> = vec![];
    ast.check_one(TokenType::Braces, String::from("{"), true, true);

    while ast
        .check_one(TokenType::Braces, String::from("}"), false, false)
        .is_none()
    {
        let el_name = expr_identifier(ast);
        ast.check_one(TokenType::Operator, String::from(":"), true, true);
        let el_type = ast.get_type_expr();

        members.push(InterfaceMember {
            span: Span {
                start: el_name.span.start,
                end: el_type.span.end,
            },
            name: el_name,
            etype: el_type,
        })
    }

    let token_end = ast
        .check_one(TokenType::Braces, String::from("}"), true, true)
        .unwrap();

    InterfaceStmt {
        ntype: String::from("InterfaceStmt"),
        name,
        members,
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
    }
}

pub fn stmt_while(ast: &mut AST) -> WhileStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("while"), true, true)
        .unwrap();

    let condition = ast.get_ast_value(true, vec![], None).unwrap();

    let block = stmt_block(ast, false);

    WhileStmt {
        ntype: String::from("WhileStmt"),
        span: Span {
            start: token_start.start,
            end: block.span.end,
        },
        condition,
        block,
    }
}

pub fn stmt_condition(ast: &mut AST) -> ConditionStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("if"), true, true)
        .unwrap();

    let condition = ast
        .get_ast_value(true, vec![String::from("DictParsed")], None)
        .unwrap();

    let block = stmt_block(ast, false);

    let mut else_stmt: Option<Box<ConditionStmt>> = None;
    if ast
        .check_one(TokenType::Keyword, String::from("else"), false, false)
        .is_some()
    {
        let else_token = ast
            .check_one(TokenType::Keyword, String::from("else"), false, false)
            .unwrap();

        if ast
            .check_one(TokenType::Keyword, String::from("if"), false, false)
            .is_some()
        {
            else_stmt = Some(Box::new(stmt_condition(ast)));
        } else {
            let block = stmt_block(ast, false);
            else_stmt = Some(Box::new(ConditionStmt {
                ntype: String::from("ConditionStmt"),
                span: Span {
                    start: else_token.start,
                    end: block.span.end,
                },
                condition: None,
                else_stmt: None,
                block,
            }));
        }
    }

    ConditionStmt {
        ntype: String::from("ConditionStmt"),
        span: Span {
            start: token_start.start,
            end: if else_stmt.is_some() {
                else_stmt.clone().unwrap().span.end
            } else {
                block.span.end
            },
        },
        block,
        condition: Some(condition),
        else_stmt,
    }
}

pub fn stmt_for(ast: &mut AST) -> ForStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("for"), true, true)
        .unwrap();

    let mut iter_val: Option<Identifier> = None;
    let mut idx_val: Option<Identifier> = None;

    if ast
        .check_token(Some(vec![TokenType::Word]), None, false, false, 0)
        .is_some()
        && ast
            .check_token(
                Some(vec![TokenType::Keyword]),
                Some(vec![String::from("of")]),
                false,
                false,
                0,
            )
            .is_some()
    {
        iter_val = Some(expr_identifier(ast));
    }

    let iterable = ast.get_ast_value(true, vec![], None).unwrap();

    if ast
        .check_one(TokenType::Keyword, String::from("at"), false, true)
        .is_some()
    {
        idx_val = Some(expr_identifier(ast));
    }

    let block = stmt_block(ast, false);

    ForStmt {
        ntype: String::from("ForStmt"),
        span: Span {
            start: token_start.start,
            end: block.span.end,
        },
        iter_val,
        idx_val,
        block,
        iterable,
    }
}

pub fn stmt_class(ast: &mut AST) -> ClassStmt {
    let token_start = ast
        .check_one(TokenType::Keyword, String::from("class"), true, true)
        .unwrap();

    let name = expr_identifier(ast);
    let mut extends: Option<Extendable> = None;

    if ast
        .check_one(TokenType::Keyword, String::from("extends"), false, true)
        .is_some()
    {
        let extend = expr_access(ast, false, false, false);

        match extend {
            Accessable::CallFunctionExpr(e) => ast.error(
                String::from("Expected class, but found Function Call Expression"),
                e.span.start.line,
                e.span.start.col,
            ),
            Accessable::AccessIndexExpr(e) => ast.error(
                String::from("Expected class, but found Access Index Expression"),
                e.span.start.line,
                e.span.start.col,
            ),
            Accessable::Identifier(e) => {
                extends = Some(Extendable::Ident(e));
            }
            Accessable::AccessDotExpr(e) => {
                extends = Some(Extendable::DotExpr(e));
            }
        }
    }

    let mut implements: Vec<Extendable> = vec![];

    let mut to_impl_check = ast
        .check_one(TokenType::Keyword, String::from("implements"), false, true)
        .is_some();
    while to_impl_check {
        let implc = expr_access(ast, false, false, false);
        let mut impls: Option<Extendable> = None;

        match implc {
            Accessable::CallFunctionExpr(e) => ast.error(
                String::from("Expected interface, but found Function Call Expression"),
                e.span.start.line,
                e.span.start.col,
            ),
            Accessable::AccessIndexExpr(e) => ast.error(
                String::from("Expected interface, but found Access Index Expression"),
                e.span.start.line,
                e.span.start.col,
            ),
            Accessable::Identifier(e) => {
                impls = Some(Extendable::Ident(e));
            }
            Accessable::AccessDotExpr(e) => {
                impls = Some(Extendable::DotExpr(e));
            }
        }

        if impls.is_some() {
            implements.push(impls.unwrap());
        }

        to_impl_check = ast
            .check_one(TokenType::Operator, String::from(","), false, true)
            .is_some()
            && ast
                .check_token(Some(vec![TokenType::Word]), None, false, false, 0)
                .is_some();
    }

    let mut initializer: Option<FunctionStmt> = None;
    let mut properties: Vec<InitVariableStmt> = vec![];
    let mut methods: Vec<FunctionStmt> = vec![];

    ast.check_one(TokenType::Braces, String::from("{"), true, true);

    while ast
        .check_one(TokenType::Braces, String::from("}"), false, false)
        .is_none()
    {
        if ast
            .check_mult(
                TokenType::Keyword,
                vec![String::from("let"), String::from("const")],
                false,
                false,
            )
            .is_some()
        {
            properties.push(stmt_init_variable(ast));
        } else {
            if ast
                .check_one(TokenType::Word, String::from("init"), false, false)
                .is_some()
            {
                initializer = Some(stmt_function(ast, false, Some(String::from("init"))));
            } else {
                methods.push(stmt_function(ast, true, None));
            }
        }
    }

    let token_end = ast
        .check_one(TokenType::Braces, String::from("}"), true, true)
        .unwrap();

    ClassStmt {
        ntype: String::from("ClassStmt"),
        span: Span {
            start: token_start.start,
            end: token_end.end,
        },
        name,
        extends,
        implements,
        initializer,
        properties,
        methods,
    }
}
