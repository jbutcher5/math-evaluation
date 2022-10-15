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
            Mul => Some(2),
            Div => Some(3),
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
            if let Some(c) = self.curr() {
                let node = match c {
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
                };

                buffer.push(node);
            }
        }

        self.find_negate_operators(buffer)
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
                panic!("Invalid Syntax.");
            }

            self.inc();
        }

        let exclusive_end = self.i - 1;

        let mut sub_lexer = Lexer::new(&self.data[start..exclusive_end]);

        Token::Brackets(sub_lexer.lex())
    }

    fn handle_number(&mut self) -> Token {
        let mut num: Vec<char> = vec![];

        while let Some(digit) = self.curr() {
            if digit.to_digit(10).is_some() || digit == '.' {
                num.push(digit);
                self.inc();
            } else {
                break;
            }
        }

        Token::Number(num.iter().collect::<String>().parse().unwrap())
    }

    fn find_negate_operators(&self, tokens: Vec<Token>) -> Vec<Token> {
        let mut new_tokens = tokens.into_iter().map(Some).collect::<Vec<Option<Token>>>();
        new_tokens.push(None);
        let mut offset = 0;

        for i in 0..new_tokens.len() - 1 {
            let i = i + offset;
            let window = (&new_tokens[i].clone(), &new_tokens[i + 1].clone());

            if i == 0 {
                if let (Some(Token::Sub), Some(Token::Brackets(contents))) = window {
                    new_tokens[i] = None;
                    new_tokens[i + i] = Some(Token::Brackets(vec![
                        Token::Number(-1.0),
                        Token::Mul,
                        Token::Brackets(contents.to_vec()),
                    ]));
                } else if let (Some(Token::Sub), Some(Token::Number(n))) = window {
                    new_tokens[i] = None;
                    new_tokens[i + 1] = Some(Token::Number(-n));
                }
            } else if let (
                Some(Token::Sub | Token::Add | Token::Mul | Token::Div),
                Some(Token::Sub),
            ) = window
            {
                if let Some(Token::Brackets(contents)) = &new_tokens[i + 2].clone() {
                    new_tokens[i + 1] = None;
                    new_tokens[i + 2] = Some(Token::Brackets(vec![
                        Token::Number(-1.0),
                        Token::Mul,
                        Token::Brackets(contents.to_vec()),
                    ]));
                } else if let Some(Token::Number(n)) = &new_tokens[i + 2].clone() {
                    new_tokens[i + 1] = None;
                    new_tokens[i + 2] = Some(Token::Number(-n));
                } else {
                    new_tokens.insert(i + 1, Some(Token::Number(-1.0)));
                    new_tokens[i + 2] = Some(Token::Mul);
                    offset += 1;
                }
            }
        }

        let new_tokens = new_tokens.into_iter().filter_map(|x| x).collect();

        new_tokens
    }
}

#[derive(Debug, Clone)]
enum Node {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Number(f64),
}

fn parse(tokens: Vec<Token>) -> Option<Node> {
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

fn eval(ast: Node) -> f64 {
    match ast {
        Node::Number(x) => x,
        Node::Add(x, y) => eval(*x) + eval(*y),
        Node::Mul(x, y) => eval(*x) * eval(*y),
        Node::Sub(x, y) => eval(*x) - eval(*y),
        Node::Div(x, y) => eval(*x) / eval(*y),
    }
}

fn calc(expr: &str) -> f64 {
    let mut lexer = Lexer::new(expr);
    let tokens = lexer.lex();
    let ast = parse(tokens).unwrap();

    eval(ast)
}

fn main() {
    let x = calc("(0.0625 + 0.1875) + (0.375 + 2 * 0.1875)");
    println!("{:?}", x);
}
