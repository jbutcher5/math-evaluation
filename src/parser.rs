use crate::lexer::Token;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub enum Node {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Number(f64),
}

pub fn parse(tokens: Vec<Token>) -> Option<Node> {
    let mut op_queue: VecDeque<Token> = VecDeque::new();
    let mut rpn: VecDeque<Token> = VecDeque::new();

    for token in tokens {
        if let Token::Number(_) | Token::Brackets(_) = token {
            rpn.push_back(token);
        } else {
            while let Some(operator) = op_queue.pop_front() {
                if operator.precedence()? >= token.precedence()? {
                    rpn.push_back(operator);
                } else {
                    op_queue.push_front(operator);
                    break;
                }
            }

            op_queue.push_front(token);
        }
    }

    while let Some(operator) = op_queue.pop_front() {
        rpn.push_back(operator);
    }

    let mut stack = vec![];

    while let Some(token) = rpn.pop_front() {
        match token {
            Token::Number(x) => stack.push(Node::Number(x)),
            Token::Brackets(contents) => stack.push(parse(contents)?),
            _ => {
                let y = stack.pop();
                let x = stack.pop();

                if let Token::Mul = token {
                    stack.push(Node::Mul(Box::new(x?), Box::new(y?)));
                } else if let Token::Add = token {
                    stack.push(Node::Add(Box::new(x?), Box::new(y?)));
                } else if let Token::Sub = token {
                    stack.push(Node::Sub(Box::new(x?), Box::new(y?)));
                } else if let Token::Div = token {
                    stack.push(Node::Div(Box::new(x?), Box::new(y?)));
                }
            }
        }
    }

    stack.last().cloned()
}
