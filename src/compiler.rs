use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::{chunk::{self, Chunk, OpCode}, value::{LoxFunction, LoxValue}, scanner::{self, Scanner}};
use scanner::{Token, TokenType};

struct Compiler<'a> {
    scanner: Rc<RefCell<Scanner<'a>>>,
    previous: Option<Token>,
    current: Option<Token>,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
    locals: Vec<Local>,
    scope_depth: i32,
    function: LoxFunction,
    function_type: FunctionType,
}

enum FunctionType {
    Function,
    Script,
}

struct Local {
    name: Token,
    depth: i32,
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

pub fn compile(source: &str) -> Option<LoxFunction> {
    let scanner = scanner::Scanner::new(source);
    let mut compiler = Compiler::new(scanner, FunctionType::Script);
    compiler.advance();

    while !compiler.matches(TokenType::Eof) {
        compiler.declaration();
    }

    compiler.end_compiler();

    if *compiler.had_error.borrow() {
        None
    } else {
        Some(compiler.function)
    }
}

impl Compiler<'_> {
    fn construct(scanner: Rc<RefCell<Scanner>>, function_type: FunctionType) -> Compiler {
        let mut c = Compiler {
            scanner,
            previous: None,
            current: None,
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
            locals: Vec::new(),
            scope_depth: 0,
            function: LoxFunction {
                name: None,
                arity: 0,
                chunk: Chunk::new(),
            },
            function_type,
        };
       /*c.locals.push(Local {
            name: Token {
                token_type: TokenType::Error,
                contents: "".to_string(),
                line: 0,
            },
            depth: 0
        });*/
        c
    }

    pub fn new(scanner: Scanner, function_type: FunctionType) -> Compiler {
        Self::construct(Rc::new(RefCell::new(scanner)), function_type)
    }

    pub fn advance(&mut self) {
        self.previous = self.current.take();
        loop {
            self.current = Some(self.scanner.borrow_mut().scan_token());
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
            panic!("{}",error_message);
            self.error_at_current(error_message);
        }
    }

    pub fn matches(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn check(&self, token_type: TokenType) -> bool {
        self.current.as_ref().unwrap().token_type == token_type
    }

    pub fn emit_opcode(&mut self, opcode: OpCode) -> usize {
        self.emit_byte(num::ToPrimitive::to_u8(&opcode).unwrap())
    }

    pub fn emit_byte(& mut self, byte: u8) -> usize {
        self.function.chunk.write_byte(byte, self.previous.as_ref().expect("Need prev token to emit byte").line)
    }

    pub fn emit_bytes(& mut self, bytes: &[u8]) {
        for byte in bytes {
            self.function.chunk.write_byte(*byte, self.previous.as_ref().expect("Need prev token to emit byte").line);
        }
    }

    pub fn emit_jump(&mut self, opcode: OpCode) -> usize {
        self.emit_opcode(opcode);
        let offset = self.emit_byte(0xff);
        self.emit_byte(0xff);
        offset
    }

    pub fn emit_constant(&mut self, constant: LoxValue) {
        self.emit_opcode(OpCode::Constant);
        let constant_id = self.make_constant(constant);
        self.emit_byte(constant_id);
    }

    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_opcode(OpCode::Loop);

        let offset = self.function.chunk.code_len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte((offset >> 8 & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    pub fn patch_jump(&mut self, offset: usize) {
        let jump = self.function.chunk.code_len() - offset - 2;
        if jump > u16::MAX as usize {
            self.error("Too much code to jump over.");
        }
        self.function.chunk.patch_byte((jump >> 8 & 0xff) as u8, offset);
        self.function.chunk.patch_byte((jump & 0xff) as u8, offset + 1);
    }

    pub fn make_constant(&mut self, constant: LoxValue) -> u8 {
        // Handle Constant pool overlfow?
        self.function.chunk.add_constant(constant)
    }

    pub fn end_compiler(&mut self) {
        self.emit_opcode(OpCode::Return);
        self.function.chunk.disassemble(self.function.name.as_deref().unwrap_or("code"));
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;

        let mut index = self.locals.len() - 1;
        while !self.locals.is_empty() && self.locals.get(index).unwrap().depth > self.scope_depth {
            self.emit_opcode(OpCode::Pop);
            self.locals.pop();
        }
    }

    pub fn expression(&mut self) {
        self.parse_precedence(PRECEDENCE_ASSIGNMENT);
    }

    pub fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_opcode(OpCode::Pop);
    }

    pub fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit_opcode(OpCode::Pop);

        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    pub fn declaration(&mut self) {
        if self.matches(TokenType::Fun) {
            self.fun_declaration();
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if *self.panic_mode.borrow() {
            self.synchronize();
        }
    }

    pub fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function mame");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    pub fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_opcode(OpCode::Nil);
        }
        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(global);
    }

    pub fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }

        let var_name = self.previous.as_ref().unwrap().contents.clone();
        self.identifier_constant(var_name)
    }

    pub fn define_variable(&mut self, id: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_opcode(OpCode::DefineGlobal);
        self.emit_byte(id);
    }

    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        let last_idx = self.locals.len() - 1;
        self.locals[last_idx].depth = self.scope_depth;
    }

    pub fn identifier_constant(&mut self, name: String) -> u8 {
        self.function.chunk.add_constant(LoxValue::String(Rc::new(name)))
    }

    pub fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        let name = self.previous.as_ref().unwrap().clone();
        for local in self.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.scope_depth {
                break;
            }
            if name.contents == local.name.contents {
                self.error("Already a variable with this name in this scope.");
            }
        }

        self.add_local(name);
    }

    pub fn add_local(&mut self, name: Token) {
        if self.locals.len() >= 255 {
            self.error("Too many local variables in function.");
            return;
        }

        self.locals.push(Local {
            name,
            depth: -1,
        });
    }

    pub fn and_(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(PRECEDENCE_AND);
        self.patch_jump(end_jump);
    }

    pub fn or_(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_opcode(OpCode::Pop);

        self.parse_precedence(PRECEDENCE_OR);
        self.patch_jump(end_jump);
    }

    pub fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    pub fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    pub fn function(&mut self, function_type: FunctionType) {
        let mut fn_compiler = Self::construct(Rc::clone(&self.scanner), function_type);
        fn_compiler.function.name = Some(self.previous.as_ref().unwrap().contents.clone());
        fn_compiler.previous = self.previous.clone();
        fn_compiler.current = self.current.clone();
        fn_compiler.begin_scope();
        fn_compiler.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !fn_compiler.check(TokenType::RightParen) {
            loop {
                fn_compiler.function.arity += 1;
                let constant = fn_compiler.parse_variable("Expect parameter name.");
                fn_compiler.define_variable(constant);
                if !self.matches(TokenType::Comma) {
                    break;;
                }
            }
        }
        fn_compiler.consume(TokenType::RightParen, "Expect ')' after parameters.");
        fn_compiler.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        fn_compiler.block();
        fn_compiler.end_compiler();

        // How should we be handling error propogation??
        self.previous = fn_compiler.previous.take();
        self.current = fn_compiler.current.take();
        let fn_obj = fn_compiler.function;
        self.emit_constant(LoxValue::Function(Rc::new(fn_obj)));
    }

    pub fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_opcode(OpCode::Print);
    }

    pub fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
            // no initializer
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.function.chunk.code_len();
        let exit_jump = if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';'.");

            // Jump out of the loop if the condition is false.
            let jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit_opcode(OpCode::Pop);
            Some(jump)
        } else {
            None
        };

        if !self.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.function.chunk.code_len();
            self.expression();
            self.emit_opcode(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit) = exit_jump {
            self.patch_jump(exit);
            self.emit_opcode(OpCode::Pop);
        }
        self.end_scope();
    }

    pub fn while_statement(&mut self) {
        let loop_start = self.function.chunk.code_len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop);
    }

    pub fn synchronize(&mut self) {
        *self.panic_mode.borrow_mut() = false;
        while self.current.as_ref().map(|t| t.token_type) != Some(TokenType::Eof) {
            if self.previous.as_ref().map(|t| t.token_type) == Some(TokenType::Semicolon) {
                return;
            }
            match self.current.as_ref().map(|t| t.token_type) {
                Some(TokenType::Class) | Some(TokenType::Fun) | Some(TokenType::Var) | Some(TokenType::For) | Some(TokenType::Fun) |
                    Some(TokenType::If) | Some(TokenType::While) | Some(TokenType::Print) | Some(TokenType::Return) => return,
                _ => ()
            }
            self.advance();
        }
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

    pub fn variable(&mut self, can_assign: bool) {
        let name = self.previous.as_ref().unwrap().contents.clone();
        self.named_variable(name, can_assign);
    }

    pub fn named_variable(&mut self, name: String, can_assign: bool) {

        let (arg, get_op, set_op) = match self.resolve_local(&name) {
            Some(idx) => (idx, OpCode::GetLocal, OpCode::SetLocal),
            None => (self.identifier_constant(name), OpCode::GetGlobal, OpCode::SetGlobal),
        };

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_opcode(set_op);
        } else {
            self.emit_opcode(get_op);
        }
        self.emit_byte(arg);
    }

    pub fn resolve_local(&self, name: &String) -> Option<u8> {
        let result_opt = self.locals.iter()
            .enumerate()
            .rev()
            .find(|(_idx, local)| local.name.contents == *name);

        result_opt.map(|(index, local)| {
            if local.depth == -1 {
                self.error("Can't read local variable in its own initializer.");
            }
            index as u8
        })
    }

    pub fn unary(&mut self) {
        let operator_type = self.previous.as_ref().unwrap().token_type;
        self.parse_precedence(PRECEDENCE_UNARY);
        match operator_type {
            TokenType::Minus => { self.emit_opcode(OpCode::Negate); },
            TokenType::Bang => { self.emit_opcode(OpCode::Not); },
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
            TokenType::EqualEqual => { self.emit_opcode(OpCode::Equal); },
            TokenType::Greater => { self.emit_opcode(OpCode::Greater); },
            TokenType::GreaterEqual => {
                self.emit_opcode(OpCode::Less);
                self.emit_opcode(OpCode::Not);
            },
            TokenType::Less => {self.emit_opcode(OpCode::Less); } ,
            TokenType::LessEqual => {
                self.emit_opcode(OpCode::Greater);
                self.emit_opcode(OpCode::Not);
            },
            TokenType::Plus => { self.emit_opcode(OpCode::Add); },
            TokenType::Minus => { self.emit_opcode(OpCode::Subtract); },
            TokenType::Star => { self.emit_opcode(OpCode::Multiply); },
            TokenType::Slash => { self.emit_opcode(OpCode::Divide); },
            _ => (),
        }
    }

    pub fn literal(&mut self) {
        match self.previous.as_ref().map(|f| f.token_type) {
            Some(TokenType::Nil) => self.emit_opcode(OpCode::Nil),
            Some(TokenType::True) => self.emit_opcode(OpCode::True),
            Some(TokenType::False) => self.emit_opcode(OpCode::False),
            _ => panic!("Unexpected literal {:?}", self.previous),
        };
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let can_assign = precedence <= PRECEDENCE_ASSIGNMENT;
        self.do_prefix(self.previous.as_ref().unwrap().token_type, can_assign);

        while precedence <= self.get_precedence(self.current.as_ref().unwrap().token_type) {
            self.advance();
            self.do_infix(self.previous.as_ref().unwrap().token_type);

            if can_assign && self.matches(TokenType::Equal) {
                self.error("Invalid assignment target");
            }
        }
    }

     pub fn do_prefix(&mut self, token_type: TokenType, can_assign: bool) {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Identifier => self.variable(can_assign),
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
            TokenType::And => self.and_(),
            TokenType::Or => self.or_(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn get_precedence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::Minus | TokenType::Plus => PRECEDENCE_TERM,
            TokenType::Slash | TokenType::Star => PRECEDENCE_FACTOR,
            TokenType::BangEqual | TokenType::EqualEqual => PRECEDENCE_EQUALITY,
            TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => PRECEDENCE_COMPARISON,
            TokenType::And => PRECEDENCE_AND,
            TokenType::Or => PRECEDENCE_OR,
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