use crate::common::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Word,
    Keyword,
    Type,
    Number,
    Float,
    String,
    Char,
    AssignmentOperator,
    ArithmeticOperator,
    LogicalOperator,
    BinaryOperator,
    UnaryOperator,
    Parenthesis,
    Braces,
    SqBraces,
    Comment,
    CommentMultiline,
    Operator,
    NewLine,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub idx: usize,
    pub ttype: TokenType,
    pub value: String,
    pub start: Position,
    pub end: Position,
}

#[derive(Debug)]
pub struct LexerOperators {
    pub assignment: Vec<String>,
    pub arithmetic: Vec<String>,
    pub unary: Vec<String>,
    pub logical: Vec<String>,
    pub binary: Vec<String>,
    pub comment: Vec<String>,
    pub comment_multiline: Vec<String>,
    pub other: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum OpType {
    Assignment,
    Arithmetic,
    Unary,
    Logical,
    Binary,
    Comment,
    CommentMultiline,
    Other,
    Unknown,
}

#[derive(Debug)]
pub struct LexerOptions {
    pub keywords: Vec<String>,
    pub types: Vec<String>,
    pub operators: LexerOperators,
}

#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
    pub line: u32,
    pub col: u32,
}

impl LexerError {
    pub fn new(msg: String, pos: Position) -> LexerError {
        LexerError {
            msg,
            line: pos.line,
            col: pos.col,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum StateType {
    None,
    Word,
    Number,
    Float,
    String,
    Parenthesis,
    Braces,
    SqBraces,
    Char,
    Operator,
    UnaryOperator,
    Comment,
    CommentMultiline,
    NewLine,
}

pub struct LexerState {
    pub stype: StateType,
    pub line: u32,
    pub col: u32,
    pub value: Vec<String>,
}

impl LexerState {
    pub fn new() -> LexerState {
        LexerState {
            line: 1,
            col: 1,
            value: Vec::new(),
            stype: StateType::None,
        }
    }

    pub fn reset(&mut self) {
        self.line = 1;
        self.col = 1;
        self.stype = StateType::None;
        self.value.clear();
    }

    pub fn push(&mut self, ch: String) {
        self.value.push(ch);
    }

    pub fn pos(&mut self, lexer: &Lexer) {
        self.line = lexer.line;
        self.col = lexer.col;
    }

    pub fn start(&mut self, lexer: &Lexer, stype: StateType) {
        self.stype = stype;
        self.pos(lexer);
        self.value.clear();
    }

    pub fn get_value(&mut self) -> String {
        self.value.join("")
    }

    pub fn make_token(
        &mut self,
        lexer: &Lexer,
        ttype: TokenType,
        results: &mut LexerResults,
    ) -> Token {
        Token {
            idx: results.tokens.len(),
            start: Position::new(self.line, self.col),
            end: Position::new(lexer.line, lexer.col),
            ttype,
            value: self.get_value(),
        }
    }

    pub fn push_token(
        &mut self,
        lexer: &Lexer,
        results: &mut LexerResults,
        ttype: TokenType,
    ) -> Token {
        let token = self.make_token(lexer, ttype, results);
        results.tokens.push(token.clone());
        token
    }

    pub fn pos_from(&mut self, pos: &Position) {
        self.line = pos.line;
        self.col = pos.col;
    }

    pub fn end(&mut self, lexer: &mut Lexer, results: &mut LexerResults) {
        match self.stype {
            StateType::Number => {
                self.push_token(lexer, results, TokenType::Number);
            }
            StateType::Float => {
                self.push_token(lexer, results, TokenType::Float);
            }
            StateType::Word => {
                if lexer.options.keywords.contains(&self.get_value()) {
                    self.push_token(lexer, results, TokenType::Keyword);
                } else {
                    self.push_token(lexer, results, TokenType::Word);
                }
            }
            StateType::String => {
                self.push_token(lexer, results, TokenType::String);
            }
            StateType::Char => {
                if self.get_value().len() != 0 {
                    results.error(lexer, "Expected char to be of length 1".to_string());
                }
                self.push_token(lexer, results, TokenType::Char);
            }
            StateType::Operator => {
                let op = lexer.check_op(self.get_value()).unwrap();

                match op {
                    OpType::Arithmetic => {
                        self.push_token(lexer, results, TokenType::ArithmeticOperator);
                    }
                    OpType::Assignment => {
                        self.push_token(lexer, results, TokenType::AssignmentOperator);
                    }
                    OpType::Logical => {
                        self.push_token(lexer, results, TokenType::LogicalOperator);
                    }
                    OpType::Binary => {
                        self.push_token(lexer, results, TokenType::BinaryOperator);
                    }
                    OpType::Unary => {
                        self.push_token(lexer, results, TokenType::UnaryOperator);
                    }
                    _ => {
                        self.push_token(lexer, results, TokenType::Operator);
                    }
                }
            }
            StateType::Comment => {
                self.push_token(lexer, results, TokenType::Comment);
            }
            StateType::CommentMultiline => {
                self.push_token(lexer, results, TokenType::CommentMultiline);
            }
            StateType::UnaryOperator => {
                self.push_token(lexer, results, TokenType::UnaryOperator);
            }
            StateType::Parenthesis => {
                self.push_token(lexer, results, TokenType::Parenthesis);
            }
            StateType::Braces => {
                self.push_token(lexer, results, TokenType::Braces);
            }
            StateType::NewLine => {
                self.push_token(lexer, results, TokenType::NewLine);
            }
            StateType::SqBraces => {
                self.push_token(lexer, results, TokenType::SqBraces);
            }
            StateType::None => {}
        }
        self.reset();
    }
}

#[derive(Debug)]
pub struct LexerResults {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexerError>,
}

impl LexerResults {
    pub fn new() -> LexerResults {
        LexerResults {
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.tokens.clear();
        self.errors.clear();
    }

    pub fn token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn error(&mut self, lexer: &Lexer, err: String) {
        self.errors.push(LexerError {
            msg: err,
            line: lexer.line,
            col: lexer.col,
        });
    }
}

pub struct Lexer {
    pub line: u32,
    pub col: u32,
    pub options: LexerOptions,
}

impl Lexer {
    pub fn new(options: LexerOptions) -> Lexer {
        Lexer {
            options,
            line: 1,
            col: 1,
        }
    }

    pub fn reset(&mut self) {
        self.line = 1;
        self.col = 1;
    }

    pub fn new_col(&mut self) {
        self.col += 1;
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn check_op(&mut self, ch: String) -> Option<OpType> {
        let ops = &self.options.operators;
        let res = if ops.arithmetic.contains(&ch) {
            OpType::Arithmetic
        } else if ops.assignment.contains(&ch) {
            OpType::Assignment
        } else if ops.binary.contains(&ch) {
            OpType::Binary
        } else if ops.comment.contains(&ch) {
            OpType::Comment
        } else if ops.comment_multiline.contains(&ch) {
            OpType::CommentMultiline
        } else if ops.logical.contains(&ch) {
            OpType::Logical
        } else if ops.unary.contains(&ch) {
            OpType::Unary
        } else if ops.other.contains(&ch) {
            OpType::Other
        } else {
            OpType::Unknown
        };

        match res {
            OpType::Unknown => Option::None,
            _ => Option::from(res),
        }
    }

    pub fn parse(&mut self, code: String) -> LexerResults {
        let mut results = LexerResults::new();
        let mut state = LexerState::new();
        let chars: Vec<char> = code.chars().collect();

        let mut idx: usize = 0;
        while idx < chars.len() {
            let ch = *chars.get(idx).unwrap();
            let peek = chars.get(idx + 1);

            if state.stype == StateType::String && ch != '"' {
                if ch == '\n' {
                    self.new_line();
                    results.error(self, "Expected end of string".to_string());
                    break;
                }
                state.push(String::from(ch));
            } else if state.stype == StateType::Comment {
                if ch == '\n' {
                    self.new_line();
                    state.end(self, &mut results);
                } else {
                    state.push(String::from(ch));
                }
            } else if state.stype == StateType::CommentMultiline {
                if ch == '*' && peek.is_some() && *peek.unwrap() == '/' {
                    idx += 1;
                    self.new_col();
                    state.end(self, &mut results);
                } else {
                    state.push(String::from(ch));
                }
            } else {
                if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' || ch == '$' {
                    if state.stype == StateType::None {
                        state.start(self, StateType::Word);
                        state.push(String::from(ch));
                    } else if state.stype == StateType::Word {
                        state.push(String::from(ch));
                    } else {
                        results.error(self, format!("Unexpected token {}", ch));
                    }
                } else if ch == ' ' {
                    self.col -= 1;
                    state.end(self, &mut results);
                    self.col += 1;
                } else if ch == '\n' {
                    if state.stype == StateType::Char {
                        results.error(self, format!("Unexpected new line in char"));
                    }
                    state.end(self, &mut results);
                    self.new_line();
                    self.col -= 1;
                    state.start(self, StateType::NewLine);
                    state.end(self, &mut results);
                } else if ch == '\r' {
                } else if ch >= '0' && ch <= '9' {
                    if state.stype == StateType::None {
                        state.start(self, StateType::Number);
                        state.push(String::from(ch));
                    } else if state.stype == StateType::Number
                        || state.stype == StateType::Float
                        || state.stype == StateType::Word
                    {
                        state.push(String::from(ch));
                    } else {
                        results.error(self, format!("Unexpected token {}", ch));
                        break;
                    }
                } else if ch == '"' {
                    if state.stype == StateType::None {
                        state.start(self, StateType::String);
                    } else if state.stype == StateType::String {
                        state.end(self, &mut results);
                    } else {
                        results.error(self, format!("Unexpected token {}", ch));
                        break;
                    }
                } else if ch == '\'' {
                    if state.stype == StateType::None {
                        state.start(self, StateType::Char);
                    } else if state.stype == StateType::Char {
                        state.end(self, &mut results);
                    } else {
                        results.error(self, format!("Unexpected token {}", ch));
                        break;
                    }
                } else if ch == '.' {
                    if state.stype == StateType::Number || state.stype == StateType::Float {
                        state.stype = StateType::Float;
                        state.push(String::from(ch));
                    } else {
                        state.end(self, &mut results);
                        state.start(self, StateType::Operator);
                        state.pos(self);
                        state.push(String::from(ch));
                        state.end(self, &mut results);
                    }
                } else if ch == ')' || ch == '(' {
                    state.end(self, &mut results);
                    state.start(self, StateType::Parenthesis);
                    state.pos(self);
                    state.push(String::from(ch));
                    state.end(self, &mut results);
                } else if ch == '{' || ch == '}' {
                    state.end(self, &mut results);
                    state.start(self, StateType::Braces);
                    state.pos(self);
                    state.push(String::from(ch));
                    state.end(self, &mut results);
                } else if ch == '[' || ch == ']' {
                    state.end(self, &mut results);
                    state.start(self, StateType::SqBraces);
                    state.pos(self);
                    state.push(String::from(ch));
                    state.end(self, &mut results);
                } else {
                    let stored_pos = Position {
                        line: self.line,
                        col: self.col,
                    };
                    state.end(self, &mut results);
                    state.pos(self);

                    let mut op_chars: String = String::new();
                    while chars.get(idx).is_some()
                        && *chars.get(idx).unwrap() != ' '
                        && op_chars.len() < 2
                        && self
                            .check_op(String::from(*chars.get(idx).unwrap()))
                            .is_some()
                    {
                        let chr = String::from(*chars.get(idx).unwrap());
                        op_chars += chr.as_str();
                        self.new_col();
                        idx += 1;
                    }

                    if op_chars == "+" || op_chars == "-" {
                        let res_tokens = &results.tokens;
                        let last_token: &Token = res_tokens.last().unwrap();
                        if last_token.ttype == TokenType::ArithmeticOperator
                            || last_token.ttype == TokenType::AssignmentOperator
                            || last_token.ttype == TokenType::BinaryOperator
                            || last_token.ttype == TokenType::LogicalOperator
                            || last_token.ttype == TokenType::UnaryOperator
                        {
                            state.start(self, StateType::UnaryOperator);
                        } else {
                            state.start(self, StateType::Operator);
                        }
                        state.push(String::from(ch));
                        state.end(self, &mut results);
                    } else {
                        let op = self.check_op(op_chars);

                        if op.is_some() {
                            let op = op.unwrap();

                            match op {
                                OpType::Arithmetic
                                | OpType::Assignment
                                | OpType::Binary
                                | OpType::Logical
                                | OpType::Unary
                                | OpType::Other => {
                                    state.start(self, StateType::Operator);
                                    let mut chrs = String::from(ch);
                                    if ch == ':'
                                        && chars.get(idx - 1).is_some()
                                        && *chars.get(idx - 1).unwrap() == '='
                                    {
                                        chrs += "=";
                                    }
                                    state.push(chrs);
                                    state.pos_from(&stored_pos);
                                    state.end(self, &mut results);
                                }

                                OpType::Comment => {
                                    state.start(self, StateType::Comment);
                                    state.pos(self);
                                }

                                OpType::CommentMultiline => {
                                    state.start(self, StateType::CommentMultiline);
                                    state.pos(self);
                                }

                                _ => {
                                    state.reset();
                                }
                            }
                        } else {
                            results.error(self, format!("Unexpected token {}", ch));
                            break;
                        }
                    }
                    continue;
                }
            }

            self.new_col();
            idx += 1;
        }

        if state.stype == StateType::Word
            || state.stype == StateType::Number
            || state.stype == StateType::Float
            || state.stype == StateType::Comment
            || state.stype == StateType::CommentMultiline
        {
            state.end(self, &mut results);
        }

        if state.stype == StateType::String {
            results.error(
                self,
                format!("Expected end of string, but got end of file instead"),
            );
        }

        if state.stype == StateType::Char {
            results.error(
                self,
                format!("Expected end of char, but got end of file instead"),
            );
        }

        results
    }
}
