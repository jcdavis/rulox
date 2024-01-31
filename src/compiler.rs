use std::{mem, borrow::Borrow, cell::RefCell};

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
    }

    pub fn number(& mut self) {
        let str = &self.previous.as_ref().unwrap().contents;
        let parsed = str.parse::<f64>();
        match parsed {
            Ok(double) => self.emit_constant(double),
            Err(err) => self.error(format!("Unable to parse {}", str).as_str()),
        }
    }

    pub fn expression(&mut self) {

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