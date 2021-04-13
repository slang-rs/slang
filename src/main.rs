pub mod ast;
pub mod common;
pub mod lexer;

use ast::main::AST;
use lexer::slang::run_lexer;

fn main() {
    let code = String::from(std::fs::read_to_string("./tests/test.sl").unwrap());
    let results = run_lexer(code);
    for token in &results.tokens {
        println!(
            "- [{:?}] {:?} [{:?}:{:?}]",
            token.ttype, token.value, token.start, token.end
        );
    }
    let mut ast = AST::new(results.tokens);
    let block = ast.get_global_block();
    println!("{:#?}", block.body);
    println!("{:#?}", ast.errors);
}
