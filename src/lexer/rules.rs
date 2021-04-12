use crate::lexer::parser::*;

pub fn get_lexer_rules() -> LexerOptions {
    LexerOptions {
        operators: LexerOperators {
            arithmetic: vec![
                "+".to_string(),
                "-".to_string(),
                "*".to_string(),
                "/".to_string(),
                "%".to_string(),
            ],
            assignment: vec![
                "=".to_string(),
                "+=".to_string(),
                "-=".to_string(),
                "/=".to_string(),
                "*=".to_string(),
                "%=".to_string(),
                "?=".to_string(),
                ":=".to_string(),
            ],
            unary: vec!["++".to_string(), "--".to_string(), "!".to_string()],
            logical: vec![
                "==".to_string(),
                "!=".to_string(),
                "&&".to_string(),
                "||".to_string(),
                "<".to_string(),
                "<=".to_string(),
                ">=".to_string(),
                ">".to_string(),
            ],
            binary: vec!["^".to_string(), "&".to_string(), "|".to_string()],
            comment: vec!["//".to_string()],
            comment_multiline: vec!["/*".to_string(), "*/".to_string()],
            other: vec![
                ":".to_string(),
                ".".to_string(),
                "??".to_string(),
                "?".to_string(),
                "(".to_string(),
                ")".to_string(),
                ",".to_string(),
                "?.".to_string(),
                ";".to_string(),
            ],
        },
        types: vec![
            "u8".to_string(),
            "i8".to_string(),
            "u16".to_string(),
            "i16".to_string(),
            "u32".to_string(),
            "u64".to_string(),
            "i32".to_string(),
            "i64".to_string(),
            "f32".to_string(),
            "f64".to_string(),
            "u32".to_string(),
            "u64".to_string(),
            "str".to_string(),
            "char".to_string(),
            "bool".to_string(),
            "null".to_string(),
            "void".to_string(),
        ],
        keywords: vec![
            "let".to_string(),
            "const".to_string(),
            "func".to_string(),
            "import".to_string(),
            "export".to_string(),
            "class".to_string(),
            "interface".to_string(),
            "true".to_string(),
            "false".to_string(),
            "extends".to_string(),
            "implements".to_string(),
            "as".to_string(),
            "while".to_string(),
            "if".to_string(),
            "else".to_string(),
            "for".to_string(),
            "assert".to_string(),
            "return".to_string(),
            "at".to_string(),
            "of".to_string(),
        ],
    }
}
