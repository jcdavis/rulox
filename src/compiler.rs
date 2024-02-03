use std::cell::RefCell;
use std::rc::Rc;

use crate::{chunk::{self, Chunk, OpCode}, value::LoxValue, scanner::{self, Scanner}};
use scanner::{Token, TokenType};

struct Compiler<'a> {
    scanner: Scanner<'a>,
    previous: Option<Token>,
    current: Option<Token>,
    chunk: chunk::Chunk,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
}

type Precedence = u8;

const PRECEDENCE_NONE: Precedence = 0;
const PRECEDENCE_ASSIGNMENT: Precedence = 1;
const PRECEDENCE_OR: Precedence = 2;
const PRECEDENCE_AND: Precedence = 3;
const PRECEDENCE_EQUALITY: Precedence = 4;
const PRECEDENCE_COMPARISON: Precedence = 5;
const PRECEDENCE_TERM: Precedence = 6;
const PRECEDENCE_FACTOR: Precedence = 7;
const PRECEDENCE_UNARY: Precedence = 8;
const PRECEDENCE_CALL: Precedence = 9;
const PRECEDENCE_PRIMARY: Precedence = 10;

pub fn compile(source: &str) -> Option<Chunk> {
    let scanner = scanner::Scanner::new(source);
    let mut compiler = Compiler::new(scanner);
    compiler.advance();
    compiler.expression();
    compiler.consume(TokenType::Eof, "Expect end of expression.");
    compiler.end_compiler();

    if *compiler.had_error.borrow() {
        None
    } else {
        Some(compiler.chunk)
    }
}

impl Compiler<'_> {
    pub fn new(scanner: Scanner) -> Compiler {
        Compiler {
            scanner,
            previous: None,
            current: None,
            chunk: chunk::Chunk::new(),
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current.take();

        loop {
            self.current = Some(self.scanner.scan_token());
            let current_token = self.current.as_ref().expect("?");
            if current_token.token_type == TokenType::Error {
                self.error_at_current(current_token.contents.as_str());
            } else {
                break;
            }
        }
    }

    pub fn consume(&mut self, expected: TokenType, error_message: &str) {
        if self.current.as_ref().is_some_and(|c| c.token_type == expected) {
            self.advance();
        } else {
            self.error_at_current(error_message);
        }
    }

    pub fn emit_opcode(&mut self, opcode: OpCode) {
        self.emit_byte(num::ToPrimitive::to_u8(&opcode).unwrap());
    }

    pub fn emit_byte(& mut self, byte: u8) {
        self.chunk.write_byte(byte, self.previous.as_ref().expect("Need prev token to emit byte").line);
    }

    pub fn emit_bytes(& mut self, bytes: &[u8]) {
        for byte in bytes {
            self.chunk.write_byte(*byte, self.previous.as_ref().expect("Need prev token to emit byte").line);
        }
    }

    pub fn emit_constant(&mut self, constant: LoxValue) {
        self.emit_opcode(OpCode::Constant);
        let constant_id = self.make_constant(constant);
        self.emit_byte(constant_id);
    }

    pub fn make_constant(&mut self, constant: LoxValue) -> u8 {
        // Handle Constant pool overlfow?
        self.chunk.add_constant(constant)
    }

    pub fn end_compiler(&mut self) {
        self.emit_opcode(OpCode::Return);
        self.chunk.disassemble("code");
    }

    pub fn expression(&mut self) {
        self.parse_precedence(PRECEDENCE_ASSIGNMENT);
    }

    pub fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    pub fn number(&mut self) {
        let str = &self.previous.as_ref().unwrap().contents;
        let parsed = str.parse::<f64>();
        match parsed {
            Ok(double) => self.emit_constant(LoxValue::Double(double)),
            Err(_err) => self.error(format!("Unable to parse {}", str).as_str()),
        }
    }

    pub fn string(&mut self) {
        let cloned_contents = self.previous.as_ref().unwrap().contents.clone();
        self.emit_constant(LoxValue::String(Rc::new(cloned_contents)));
    }

    pub fn unary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().token_type;
        self.parse_precedence(PRECEDENCE_UNARY);
        match operator_type {
            TokenType::Minus => self.emit_opcode(OpCode::Negate),
            TokenType::Bang => self.emit_opcode(OpCode::Not),
            _ => (),
        }
    }

    pub fn binary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().token_type;
        let precedence = self.get_precedence(operator_type);
        self.parse_precedence(precedence + 1); // +1 somehow

        match operator_type {
            TokenType::BangEqual => {
                self.emit_opcode(OpCode::Equal);
                self.emit_opcode(OpCode::Not);
            },
            TokenType::EqualEqual => self.emit_opcode(OpCode::Equal),
            TokenType::Greater => self.emit_opcode(OpCode::Greater),
            TokenType::GreaterEqual => {
                self.emit_opcode(OpCode::Less);
                self.emit_opcode(OpCode::Not);
            },
            TokenType::Less => self.emit_opcode(OpCode::Less),
            TokenType::LessEqual => {
                self.emit_opcode(OpCode::Greater);
                self.emit_opcode(OpCode::Not);
            },
            TokenType::Plus => self.emit_opcode(OpCode::Add),
            TokenType::Minus => self.emit_opcode(OpCode::Subtract),
            TokenType::Star => self.emit_opcode(OpCode::Multiply),
            TokenType::Slash => self.emit_opcode(OpCode::Divide),
            _ => (),
        }
    }

    pub fn literal(&mut self) {
        match self.previous.as_ref().map(|f| f.token_type) {
            Some(TokenType::Nil) => self.emit_opcode(OpCode::Nil),
            Some(TokenType::True) => self.emit_opcode(OpCode::True),
            Some(TokenType::False) => self.emit_opcode(OpCode::False),
            _ => panic!("Unexpected literal {:?}", self.previous),
        }
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        self.do_prefix(self.previous.as_ref().unwrap().token_type);

        while precedence <= self.get_precedence(self.current.as_ref().unwrap().token_type) {
            self.advance();
            self.do_infix(self.previous.as_ref().unwrap().token_type);
        }
    }

     pub fn do_prefix(&mut self, token_type: TokenType) {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::String => self.string(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn do_infix(&mut self, token_type: TokenType) {
        match token_type {
            TokenType::Minus | TokenType::Plus | TokenType::Slash | TokenType::Star => self.binary(),
            TokenType::BangEqual | TokenType::EqualEqual | TokenType::Greater | TokenType::GreaterEqual |
                TokenType::Less | TokenType::LessEqual => self.binary(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn get_precedence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::Minus | TokenType::Plus => PRECEDENCE_TERM,
            TokenType::Slash | TokenType::Star => PRECEDENCE_FACTOR,
            TokenType::BangEqual | TokenType::EqualEqual => PRECEDENCE_EQUALITY,
            TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => PRECEDENCE_COMPARISON,
            _ => PRECEDENCE_NONE,
        }
     }

    fn error_at_current(&self, message: &str) {
        self.error_at(&self.current, message);
    }

    fn error(&self, message: &str) {
        let token = &self.previous;
        self.error_at(token, message);
    }

    fn error_at(&self, token: &Option<Token>, message: &str) {
        if *self.panic_mode.borrow() {
            return;
        }
        *self.panic_mode.borrow_mut() = true;
        let unwrapped = token.as_ref().expect("Reporting error on empty token?");
        print!("[Line {}] Error", unwrapped.line);

        match unwrapped.token_type {
            TokenType::Eof => print!(" at end."),
            TokenType::Error => (),
            _ => print!(" at {}", unwrapped.contents),
        }
        println!(" {}", message);
        *self.had_error.borrow_mut() = true;
    }
}