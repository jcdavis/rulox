use std::{borrow::Borrow, cell::RefCell};

use crate::{chunk::{self, Chunk, OpCode}, scanner::{self, Scanner}};
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
    compiler.consume(TokenType::TOKEN_EOF, "Expect end of expression.");
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
            if current_token.token_type == TokenType::TOKEN_ERROR {
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

    pub fn emit_constant(&mut self, constant: f64) {
        self.emit_opcode(OpCode::Constant);
        let constant_id = self.make_constant(constant);
        self.emit_byte(constant_id);
    }

    pub fn make_constant(&mut self, constant: f64) -> u8 {
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
        self.consume(TokenType::TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    pub fn number(&mut self) {
        let str = &self.previous.as_ref().unwrap().contents;
        let parsed = str.parse::<f64>();
        match parsed {
            Ok(double) => self.emit_constant(double),
            Err(_err) => self.error(format!("Unable to parse {}", str).as_str()),
        }
    }

    pub fn unary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().token_type;
        self.parse_precedence(PRECEDENCE_UNARY);
        match operator_type {
            TokenType::TOKEN_MINUS => self.emit_opcode(OpCode::Negate),
            _ => (),
        }
    }

    pub fn binary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().token_type;
        let precedence = self.get_precedence(operator_type);
        self.parse_precedence(precedence + 1); // +1 somehow

        match operator_type {
            TokenType::TOKEN_PLUS => self.emit_opcode(OpCode::Add),
            TokenType::TOKEN_MINUS => self.emit_opcode(OpCode::Subtract),
            TokenType::TOKEN_STAR => self.emit_opcode(OpCode::Multiply),
            TokenType::TOKEN_SLASH => self.emit_opcode(OpCode::Divide),
            _ => (),
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
            TokenType::TOKEN_LEFT_PAREN => self.grouping(),
            TokenType::TOKEN_MINUS => self.unary(),
            TokenType::TOKEN_NUMBER => self.number(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn do_infix(&mut self, token_type: TokenType) {
        match token_type {
            TokenType::TOKEN_MINUS | TokenType::TOKEN_PLUS | TokenType::TOKEN_SLASH | TokenType::TOKEN_STAR => self.binary(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn get_precedence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::TOKEN_MINUS | TokenType::TOKEN_PLUS => PRECEDENCE_TERM,
            TokenType::TOKEN_SLASH | TokenType::TOKEN_STAR => PRECEDENCE_FACTOR,
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
            TokenType::TOKEN_EOF => print!(" at end."),
            TokenType::TOKEN_ERROR => (),
            _ => print!(" at {}", unwrapped.contents),
        }
        println!(" {}", message);
        *self.had_error.borrow_mut() = true;
    }
}