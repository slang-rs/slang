pub mod ast;
pub mod common;
pub mod lexer;

use lexer::slang::run_lexer;

fn main() {
    let code = String::from(std::fs::read_to_string("./test.sl").unwrap());
    let results = run_lexer(code);
    for token in &results.tokens {
        println!(
            "- [{:?}] {:?} [{:?}:{:?}]",
            token.ttype, token.value, token.start, token.end
        );
    }
}
