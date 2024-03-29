use std::cell::RefCell;
use std::rc::Rc;

use crate::{chunk::{Chunk, OpCode}, scanner::{self, Scanner}, value::{LoxClosure, LoxFunction, LoxValue}};
use scanner::{Token, TokenType};

struct Compiler<'a, 'b> {
    scanner: Rc<RefCell<Scanner<'a>>>,
    had_error: RefCell<bool>,
    panic_mode: RefCell<bool>,
    locals: Vec<Local>,
    scope_depth: i32,
    function: LoxFunction,
    function_type: FunctionType,
    parent: Option<&'b Compiler<'a, 'b>>,
    upvalues: RefCell<Vec<UpValue>>,
    function_upvalue_count: RefCell<usize>,
    debug: bool,
    class_compilers: Vec<ClassCompiler>,
}

#[derive(PartialEq)]
enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

struct Local {
    name: Token,
    depth: i32,
    is_captured: RefCell<bool>,
}

#[derive(Debug, PartialEq)]
struct UpValue {
    idx: u8,
    is_local: bool,
}

#[derive(Clone, Debug)]
struct ClassCompiler {
    has_superclass: bool,
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

pub fn compile(source: &str, debug: bool) -> Option<LoxFunction> {
    let scanner = scanner::Scanner::new(source);
    let mut compiler = Compiler::new(scanner, FunctionType::Script, debug);
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

impl<'a, 'b> Compiler<'a, 'b> {
    fn construct<'c>(scanner: Rc<RefCell<Scanner<'a>>>, function_type: FunctionType, parent: Option<&'c Compiler<'a, 'b>>, debug: bool) -> Compiler<'a, 'c> {
        let mut locals = Vec::new();
        if function_type != FunctionType::Function {
            locals.push(Local {
                name: Token {
                    token_type: TokenType::This,
                    contents: "this".to_string(),
                    line: 0,
                },
                depth: 0,
                is_captured: RefCell::new(false),
            });
        } else {
            locals.push(Local {
                name: Token {
                    token_type: TokenType::Error,
                    contents: "".to_string(),
                    line: 0,
                },
                depth: 0,
                is_captured: RefCell::new(false),
            });
        }
        Compiler {
            scanner,
            had_error: RefCell::new(false),
            panic_mode: RefCell::new(false),
            locals,
            scope_depth: 0,
            function: LoxFunction {
                name: None,
                arity: 0,
                chunk: Chunk::new(),
            },
            function_type,
            parent,
            upvalues: RefCell::new(Vec::new()),
            function_upvalue_count: RefCell::new(0),
            debug,
            class_compilers: parent.map(|p| p.class_compilers.clone()).unwrap_or_default(),
        }
    }

    pub fn new(scanner: Scanner<'a>, function_type: FunctionType, debug: bool) -> Compiler<'a, 'b> {
        Self::construct(Rc::new(RefCell::new(scanner)), function_type, None, debug)
    }

    pub fn advance(&mut self) {
        loop {
            let mut scanner = self.scanner.borrow_mut();
            scanner.advance_token();
            let current_token = scanner.current_token().expect("?");
            if current_token.token_type == TokenType::Error {
                self.error_at_current(current_token.contents.as_str());
            } else {
                break;
            }
        }
    }

    pub fn consume(&mut self, expected: TokenType, error_message: &str) {
        if self.scanner.borrow().current_token().is_some_and(|c| c.token_type == expected) {
            self.advance();
        } else {
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
        self.scanner.borrow().current_token().unwrap().token_type == token_type
    }

    pub fn emit_opcode(&mut self, opcode: OpCode) -> usize {
        self.emit_byte(num::ToPrimitive::to_u8(&opcode).unwrap())
    }

    pub fn emit_byte(& mut self, byte: u8) -> usize {
        self.function.chunk.write_byte(byte, self.scanner.borrow().previous_token().expect("Need prev token to emit byte").line)
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

    pub fn emit_return(&mut self) {
        if self.function_type == FunctionType::Initializer {
            self.emit_opcode(OpCode::GetLocal);
            self.emit_byte(0);
        } else {
            self.emit_opcode(OpCode::Nil);
        }
        self.emit_opcode(OpCode::Return);
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
        self.emit_return();
        if self.debug {
            self.function.chunk.disassemble(self.function.name.as_deref().unwrap_or("code"));
        }
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            let local = self.locals.pop().unwrap();
            if *local.is_captured.borrow() {
                self.emit_opcode(OpCode::CloseUpValue);
            } else {
                self.emit_opcode(OpCode::Pop);
            }
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

    pub fn return_statement(&mut self) {
        if self.function_type == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }

        if self.matches(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if self.function_type == FunctionType::Initializer {
                self.error("Can't return a value from an initializer.");
            }
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_opcode(OpCode::Return);
        }
    }

    pub fn declaration(&mut self) {
        if self.matches(TokenType::Class) {
            self.class_declaration();
        } else if self.matches(TokenType::Fun) {
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

    pub fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.scanner.borrow().previous_token().unwrap().contents.clone();
        let name_constant = self.identifier_constant(class_name.clone());
        self.declare_variable();

        self.emit_opcode(OpCode::Class);
        self.emit_byte(name_constant);
        self.define_variable(name_constant);

        self.class_compilers.push(ClassCompiler { has_superclass: false});

        if self.matches(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            self.variable(false);

            if self.scanner.borrow().previous_token().map(|t| &t.contents) == Some(&class_name) {
                self.error("A class can't inherit from itself.");
            }

            self.begin_scope();
            self.add_local(Token {
                token_type: TokenType::Super,
                contents: "super".to_string(),
                line: 0,
            });
            self.define_variable(0);

            self.named_variable(class_name.clone(), false);
            self.emit_opcode(OpCode::Inherit);
            self.class_compilers.last_mut().unwrap().has_superclass = true;
        }

        self.named_variable(class_name, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_opcode(OpCode::Pop); // Class named variable

        let prev = self.class_compilers.pop().unwrap();

        if prev.has_superclass {
            self.end_scope();
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

        let var_name = self.scanner.borrow().previous_token().unwrap().contents.clone();
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

        let name = self.scanner.borrow().previous_token().unwrap().clone();
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
            is_captured: RefCell::new(false),
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
        } else if self.matches(TokenType::Return) {
            self.return_statement();
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
        let name = Some(self.scanner.borrow().previous_token().unwrap().contents.clone());
        let (function, upvalues, upvalue_count) = {
            let rc = Rc::clone(&self.scanner);
            let mut fn_compiler = Self::construct(rc, function_type, Some(self), self.debug);
            fn_compiler.function.name = name;
            fn_compiler.begin_scope();
            fn_compiler.consume(TokenType::LeftParen, "Expect '(' after function name.");
            if !fn_compiler.check(TokenType::RightParen) {
                loop {
                    fn_compiler.function.arity += 1;
                    let constant = fn_compiler.parse_variable("Expect parameter name.");
                    fn_compiler.define_variable(constant);
                    if !fn_compiler.matches(TokenType::Comma) {
                        break;
                    }
                }
            }
            fn_compiler.consume(TokenType::RightParen, "Expect ')' after parameters.");
            fn_compiler.consume(TokenType::LeftBrace, "Expect '{' before function body.");
            fn_compiler.block();
            fn_compiler.end_compiler();
            (fn_compiler.function, fn_compiler.upvalues.take(), fn_compiler.function_upvalue_count.take())
        };

        // End scope??
        // How should we be handling error propogation??
        self.emit_opcode(OpCode::Closure);
        let closure = LoxClosure {
            function,
            upvalue_count,
            upvalues: RefCell::new(Vec::new()),
        };
        let constant_id = self.make_constant(LoxValue::Closure(Rc::new(closure)));
        self.emit_byte(constant_id);

        for uv in upvalues.iter().take(upvalue_count) {
            self.emit_byte(uv.is_local as u8);
            self.emit_byte(uv.idx);
        }
    }

    pub fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let name = self.scanner.borrow().previous_token().unwrap().contents.clone();
        let constant = self.identifier_constant(name);

        let function_type = if self.scanner.borrow().previous_token().map(|t| t.contents.as_str())  == Some("init") {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(function_type);
        self.emit_opcode(OpCode::Method);
        self.emit_byte(constant);
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
        while self.scanner.borrow().current_token().map(|t| t.token_type) != Some(TokenType::Eof) {
            if self.scanner.borrow().previous_token().map(|t| t.token_type) == Some(TokenType::Semicolon) {
                return;
            }
            match self.scanner.borrow().current_token().map(|t| t.token_type) {
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
        let parsed = {
            let binding = self.scanner.borrow();
            let str = &binding.previous_token().unwrap().contents;
            str.parse::<f64>()
        };
        match parsed {
            Ok(double) => self.emit_constant(LoxValue::Double(double)),
            Err(_err) => {
                let binding = self.scanner.borrow();
                let workaround = &binding.previous_token().unwrap().contents;
                self.error(format!("Unable to parse {}", workaround).as_str());
            },
        };
    }

    pub fn string(&mut self) {
        let cloned_contents = self.scanner.borrow().previous_token().unwrap().contents.clone();
        self.emit_constant(LoxValue::String(Rc::new(cloned_contents)));
    }

    pub fn variable(&mut self, can_assign: bool) {
        let name = self.scanner.borrow().previous_token().unwrap().contents.clone();
        self.named_variable(name, can_assign);
    }

    pub fn this(&mut self) {
        if self.class_compilers.is_empty() {
            self.error("Cant use 'this' outside of a class.");
        } else {
            self.variable(false);
        }
    }

    pub fn super_(&mut self) {
        if let Some(cc) = self.class_compilers.last() {
            if !cc.has_superclass {
                self.error("Can't use 'super' in a class with no superclass.");
            }
        } else {
            self.error("Can't use 'super' outside of a class.");
        }
        self.consume(TokenType::Dot, "Expect '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expect superclass method name.");
        let cloned_contents = self.scanner.borrow().previous_token().unwrap().contents.clone();
        let name = self.identifier_constant(cloned_contents);

        self.named_variable("this".to_string(), false);
        if self.matches(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable("super".to_string(), false);
            self.emit_opcode(OpCode::SuperInvoke);
            self.emit_byte(name);
            self.emit_byte(arg_count);
        } else {
            self.named_variable("super".to_string(), false);
            self.emit_opcode(OpCode::GetSuper);
            self.emit_byte(name);
        }
    }

    pub fn named_variable(&mut self, name: String, can_assign: bool) {

        let (arg, get_op, set_op) = if let Some(idx) = self.resolve_local(&name) {
            (idx, OpCode::GetLocal, OpCode::SetLocal)
        } else if let Some(idx) = self.resolve_upvalue(&name) {
            (idx, OpCode::GetUpvalue, OpCode::SetUpvalue)
        } else {
            (self.identifier_constant(name), OpCode::GetGlobal, OpCode::SetGlobal)
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

    pub fn resolve_upvalue(&self, name: &String) -> Option<u8> {
        self.parent
            .as_ref()
            .and_then(|p| {
                p.resolve_local(name).map(|idx| {
                    *p.locals[idx as usize].is_captured.borrow_mut() = true;
                    self.add_upvalue(idx, true)
                })
                .or_else(|| { p.resolve_upvalue(name).map(|idx| self.add_upvalue(idx, false))})
            })
    }

    pub fn add_upvalue(&self, idx: u8, is_local: bool) -> u8 {
        let upvalue = UpValue { idx, is_local};

        let mut binding = self.upvalues.borrow_mut();
        let existing_uv = binding
            .iter()
            .position(|x| x == &upvalue)
            .map(|us| us as u8);
        existing_uv.unwrap_or_else(|| {
            if binding.len() == u8::MAX as usize {
                self.error("Too many closure variables in function.");
                0
            } else {
                *self.function_upvalue_count.borrow_mut() += 1;
                binding.push(upvalue);
                binding.len() as u8 - 1
            }
        })
    }

    pub fn unary(&mut self) {
        let operator_type = self.scanner.borrow().previous_token().unwrap().token_type;
        self.parse_precedence(PRECEDENCE_UNARY);
        match operator_type {
            TokenType::Minus => { self.emit_opcode(OpCode::Negate); },
            TokenType::Bang => { self.emit_opcode(OpCode::Not); },
            _ => (),
        }
    }

    pub fn binary(&mut self) {
        let operator_type = self.scanner.borrow().previous_token().unwrap().token_type;
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

    pub fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.scanner.borrow().previous_token().unwrap().contents.clone();
        let name_id = self.identifier_constant(name);

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_opcode(OpCode::SetProperty);
            self.emit_byte(name_id);
        } else if self.matches(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.emit_opcode(OpCode::Invoke);
            self.emit_byte(name_id);
            self.emit_byte(arg_count);
        } else {
            self.emit_opcode(OpCode::GetProperty);
            self.emit_byte(name_id);
        }
    }

    pub fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_opcode(OpCode::Call);
        self.emit_byte(arg_count);
    }

    pub fn argument_list(&mut self) -> u8 {
        let mut arg_count: u8 = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    pub fn literal(&mut self) {
        let token_type = self.scanner.borrow().previous_token().map(|f| f.token_type);
        match token_type {
            Some(TokenType::Nil) => self.emit_opcode(OpCode::Nil),
            Some(TokenType::True) => self.emit_opcode(OpCode::True),
            Some(TokenType::False) => self.emit_opcode(OpCode::False),
            _ => panic!("Unexpected literal {:?}", self.scanner.borrow().previous_token()),
        };
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let can_assign = precedence <= PRECEDENCE_ASSIGNMENT;
        let token_type = self.scanner.borrow().previous_token().unwrap().token_type;
        self.do_prefix(token_type, can_assign);

        while precedence <= self.get_precedence(self.scanner.borrow().current_token().unwrap().token_type) {
            self.advance();
            let token_type = self.scanner.borrow().previous_token().unwrap().token_type;
            self.do_infix(token_type, can_assign);

            if can_assign && self.matches(TokenType::Equal) {
                self.error("Invalid assignment target");
            }
        };
    }

     pub fn do_prefix(&mut self, token_type: TokenType, can_assign: bool) {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Identifier => self.variable(can_assign),
            TokenType::String => self.string(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            TokenType::This => self.this(),
            TokenType::Super => self.super_(),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn do_infix(&mut self, token_type: TokenType, can_assign: bool) {
        match token_type {
            TokenType::LeftParen => self.call(),
            TokenType::Minus | TokenType::Plus | TokenType::Slash | TokenType::Star => self.binary(),
            TokenType::BangEqual | TokenType::EqualEqual | TokenType::Greater | TokenType::GreaterEqual |
                TokenType::Less | TokenType::LessEqual => self.binary(),
            TokenType::And => self.and_(),
            TokenType::Or => self.or_(),
            TokenType::Dot => self.dot(can_assign),
            rest => self.error(format!("Expect expression, got {:?}", rest).as_str()),
        }
     }

     pub fn get_precedence(&self, token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::LeftParen | TokenType::Dot => PRECEDENCE_CALL,
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
        self.error_at(self.scanner.borrow().current_token(), message);
    }

    fn error(&self, message: &str) {
        self.error_at(self.scanner.borrow().previous_token(), message);
    }

    fn error_at(&self, token: Option<&Token>, message: &str) {
        if *self.panic_mode.borrow() {
            return;
        }
        *self.panic_mode.borrow_mut() = true;
        let unwrapped = token.expect("Reporting error on empty token?");
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