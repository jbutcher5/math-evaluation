pub mod eval;
pub mod lexer;
pub mod parser;

pub fn calc(expr: &str) -> f64 {
    let mut lexer = lexer::Lexer::new(expr);
    let tokens = lexer.lex();
    let ast = parser::parse(tokens).unwrap();

    eval::eval(ast)
}
