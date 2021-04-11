use crate::lexer::parser::*;
use crate::lexer::rules::*;

pub fn run_lexer(code: String) -> LexerResults {
    let mut lexer = Lexer::new(get_lexer_rules());
    lexer.parse(code)
}
