use crate::parser::Node;

pub fn eval(ast: Node) -> f64 {
    match ast {
        Node::Number(x) => x,
        Node::Add(x, y) => eval(*x) + eval(*y),
        Node::Mul(x, y) => eval(*x) * eval(*y),
        Node::Sub(x, y) => eval(*x) - eval(*y),
        Node::Div(x, y) => eval(*x) / eval(*y),
    }
}
