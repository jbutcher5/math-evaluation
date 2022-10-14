use std::collections::VecDeque;

#[derive(Clone, Debug)]
enum Token {
    Add,
    Sub,
    Mul,
    Div,
    Number(f64),
    Brackets(Vec<Token>),
}

impl Token {
    pub fn precedence(&self) -> Option<u8> {
        use Token::*;

        match self {
            Add | Sub => Some(1),
            Mul | Div => Some(2),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
struct Lexer {
    i: usize,
    data: String,
}

impl Lexer {
    fn new(expr: &str) -> Self {
        Lexer {
            i: 0,
            data: expr.to_string(),
        }
    }

    fn curr(&mut self) -> Option<char> {
        self.data.chars().nth(self.i)
    }

    fn skip_whitespace(&mut self) {
        while let Some(' ' | '\n') = self.curr() {
            self.inc();
        }
    }

    fn inc(&mut self) {
        self.i += 1;
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut buffer = vec![];

        while self.i <= self.data.len() - 1 {
            self.skip_whitespace();
            let node = if let Some(c) = self.curr() {
                match c {
                    '(' => self.handle_brackets(),
                    ('0'..='9') => self.handle_number(),
                    '+' => {
                        self.inc();
                        Token::Add
                    }
                    '*' => {
                        self.inc();
                        Token::Mul
                    }
                    '-' => {
                        self.inc();
                        Token::Sub
                    }
                    '/' => {
                        self.inc();
                        Token::Div
                    }
                    _ => panic!("Unkown character: `{c}`"),
                }
            } else {
                panic!("Invalid Syntax.");
            };

            buffer.push(node);
        }

        buffer
    }

    fn handle_brackets(&mut self) -> Token {
        let mut trace = 1;
        self.inc();

        let start = self.i;

        while trace != 0 {
            self.skip_whitespace();
            if let Some('(') = self.curr() {
                trace += 1;
            } else if let Some(')') = self.curr() {
                trace -= 1;
            } else if let None = self.curr() {
                panic!("Invalid Syntax");
            }

            self.inc();
        }

        let exclusive_end = self.i - 1;

        let mut sub_lexer = Lexer::new(&self.data[start..exclusive_end]);

        Token::Brackets(sub_lexer.lex())
    }

    fn handle_number(&mut self) -> Token {
        let mut num: f64 = 0.0;
        let mut decimal_places: i32 = -1;

        while let Some(digit) = self.curr() {
            if let Some(value) = digit.to_digit(10) {
                if decimal_places < 0 {
                    num *= 10.0;
                    num += value as f64;
                } else {
                    decimal_places += 1;
                    num += (0.1_f64).powi(decimal_places) * value as f64;
                }

                self.inc();
            } else if digit == '.' && decimal_places < 0 {
                decimal_places = 0;
                self.inc();
            } else {
                break;
            }
        }

        Token::Number(num)
    }
}

#[derive(Debug, Clone)]
enum Node {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Negate(Box<Node>),
    Number(f64),
}

fn parse(tokens: Vec<Token>) -> Option<Node> {
    let mut op_queue: VecDeque<Token> = VecDeque::new();
    let mut rpn: VecDeque<Token> = VecDeque::new();

    for token in tokens {
        if let Token::Number(_) = token {
            rpn.push_back(token);
        } else if let Token::Brackets(_contents) = token {
            todo!();
        } else {
            while let Some(operator) = op_queue.pop_front() {
                if operator.precedence()? > token.precedence()? {
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
            _ => {
                let x = Box::new(stack.pop()?);
                let y = Box::new(stack.pop()?);

                if let Token::Mul = token {
                    stack.push(Node::Mul(x, y));
                } else if let Token::Add = token {
                    stack.push(Node::Add(x, y));
                } else if let Token::Sub = token {
                    stack.push(Node::Sub(x, y));
                } else if let Token::Div = token {
                    stack.push(Node::Div(x, y));
                }
            }
        }
    }

    stack.last().cloned()
}

fn calc(expr: &str) -> f64 {
    let mut lexer = Lexer::new(expr);
    let tokens = lexer.lex();
    println!("{:?}", parse(tokens));
    todo!()
}

fn main() {
    println!("{:?}", calc("1+2*3-4"));
}
