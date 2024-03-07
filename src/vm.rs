extern crate num;

use std::cell::RefCell;
use std::{collections::HashMap, rc::Rc};

use crate::{chunk, value::{LoxClosure, LoxFunction, LoxValue}};
use chunk::OpCode;

struct CallFrame {
    closure: Rc<LoxClosure>,
    ip: usize,
    stack_offset: usize,
}

impl CallFrame {
    fn read_constant(&mut self) -> &LoxValue {
        let new_offset = self.read_byte() as usize;
        self.closure.function.chunk.read_constant(new_offset)
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.closure.function.chunk.read_byte(self.ip);
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        ((self.read_byte() as u16) << 8) + (self.read_byte() as u16)
    }
}

#[derive(Debug)]
pub enum UpValue {
    Open(usize),
    Closed(RefCell<LoxValue>),
}

pub struct VM {
    debug: bool,
    frames: Vec<CallFrame>,
    stack: Vec<Rc<LoxValue>>,
    globals: HashMap<String, Rc<LoxValue>>,
    upvalues: Vec<UpValue>,
}

impl VM {
    pub fn new(script: LoxFunction) -> VM {
        let mut vm = VM {
            debug: false,
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            upvalues: Vec::new()
        };
        let closure = Rc::new(LoxClosure {
            function: script,
            upvalue_count: 0,
            upvalues: RefCell::new(Vec::new()),
        });
        vm.stack.push(Rc::new(LoxValue::Closure(closure.clone())));
        vm.call(closure, 0);
        vm
    }

    pub fn debug(&mut self) {
        self.debug = true;
    }

    pub fn run(&mut self) -> u8 {
        loop {
            if self.debug {
                println!("=== Stack === ");
                for value in &self.stack {
                    println!("{}", value);
                }
                self.get_current_frame().closure.function.chunk.disassemble_instruction(self.get_current_ip());
            }
            let inst: OpCode = num::FromPrimitive::from_u8(self.get_current_frame_mut().read_byte()).unwrap();
            match inst {
                OpCode::Constant => {
                    let constant = self.get_current_frame_mut().read_constant().clone();
                    self.push(constant);
                },
                OpCode::Nil => self.push(LoxValue::Nil),
                OpCode::True => self.push(LoxValue::Bool(true)),
                OpCode::False => self.push(LoxValue::Bool(false)),
                OpCode::Pop => { self.pop(); },
                OpCode::GetLocal => {
                    let slot = self.get_current_frame_mut().read_byte() as usize + self.get_current_frame().stack_offset + 1;

                    self.push_rc(Rc::clone(&self.stack[slot]));
                },
                OpCode::SetLocal => {
                    let slot = self.get_current_frame_mut().read_byte() as usize + self.get_current_frame().stack_offset + 1;
                    self.stack[slot] = Rc::clone(self.peek(0));
                },
                OpCode::GetGlobal => {
                    match self.get_current_frame_mut().read_constant().as_string() {
                        Some(name) => {
                            match self.globals.get(&name) {
                                Some(value) => self.push_rc(Rc::clone(value)),
                                None => {
                                    self.runtime_error(&format!("Undefined variable {}", name));
                                    return 1;
                                },
                            }
                        },
                        None => {
                            self.runtime_error("Name constant is not a string!");
                            return 1;
                        }
                    }
                },
                OpCode::DefineGlobal => {
                    match self.get_current_frame_mut().read_constant().as_string() {
                        Some(name) => {
                            let value = self.pop();
                            self.globals.insert(name, value);
                        }
                        None => {
                            self.runtime_error("Name constant is not a string!");
                            return 1;
                        }
                    }
                },
                OpCode::SetGlobal => {
                    match self.get_current_frame_mut().read_constant().as_string() {
                        Some(name) => {
                            let value = self.peek(0);
                            if self.globals.contains_key(&name) {
                                self.globals.insert(name, Rc::clone(value));
                            } else {
                                self.runtime_error(format!("Undefined variable '{}'", name).as_str());
                                return 1;
                            }
                        }
                        None => {
                            self.runtime_error("Name constant is not a string!");
                            return 1;
                        }
                    }
                },
                OpCode::GetUpvalue => {
                    let slot = self.get_current_frame_mut().read_byte() as usize;
                    let uv_idx = self.get_current_frame().closure.upvalues.borrow()[slot];
                    let rc = match &self.upvalues[uv_idx] {
                        UpValue::Open(idx) => Rc::clone(&self.stack[*idx]),
                        UpValue::Closed(rc) => Rc::new(rc.borrow().clone()),
                    };
                    self.push_rc(rc);
                },
                OpCode::SetUpvalue => {
                    let slot = self.get_current_frame_mut().read_byte() as usize;
                    let uv_idx = self.get_current_frame().closure.upvalues.borrow_mut()[slot];
                    let uv = &self.upvalues[uv_idx];
                    let new_rc = Rc::clone(self.peek(0));
                    match uv {
                        UpValue::Open(idx) => self.stack[*idx] =  new_rc,
                        UpValue::Closed(rc) => *rc.borrow_mut() = (*new_rc).clone(),
                    }
                },
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(LoxValue::Bool(self.values_equal(&a,&b)))
                },
                OpCode::Greater => {
                    match (self.peek(0).as_double(), self.peek(1).as_double()) {
                        (Some(a), Some(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Bool(b > a));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Less => {
                    match (self.peek(0).as_double(), self.peek(1).as_double()) {
                        (Some(a), Some(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Bool(b < a));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Add => {
                    match (Rc::clone(self.peek(0)).as_ref(), Rc::clone(self.peek(1)).as_ref()) {
                        (LoxValue::Double(a), LoxValue::Double(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Double(a + b));
                        },
                        (LoxValue::String(a), LoxValue::String(b)) => {
                            self.pop();
                            self.pop();
                            let mut string = (**b).clone();
                            string.push_str(a);
                            self.push(LoxValue::String(Rc::new(string)));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Subtract => {
                    match (self.peek(0).as_double(), self.peek(1).as_double()) {
                        (Some(a), Some(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Double(b - a));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Multiply => {
                    match (self.peek(0).as_double(), self.peek(1).as_double()) {
                        (Some(a), Some(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Double(a * b));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Divide => {
                    match (self.peek(0).as_double(), self.peek(1).as_double()) {
                        (Some(a), Some(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Double(b / a));
                        },
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return 1;
                        },
                    }
                }
                OpCode::Not => {
                    let value = self.pop();
                    let inverted = Self::is_falsey(&value);
                    self.push(LoxValue::Bool(inverted));
                },
                OpCode::Negate => {
                    match self.peek(0).as_double() {
                        Some(d) => {
                            self.pop();
                            self.push(LoxValue::Double(-d));
                        }
                        _ => {
                            self.runtime_error("Operand must be a number.");
                            return 1;
                        },
                    }
                }
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Jump => {
                    let offset = self.get_current_frame_mut().read_short();
                    self.get_current_frame_mut().ip += offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.get_current_frame_mut().read_short();
                    if Self::is_falsey(self.peek(0)) {
                        self.get_current_frame_mut().ip += offset as usize;
                    }
                },
                OpCode::Loop => {
                    let offset = self.get_current_frame_mut().read_short();
                    self.get_current_frame_mut().ip -= offset as usize;
                },
                OpCode::Call => {
                    let arg_count = self.get_current_frame_mut().read_byte();
                    let call_value = Rc::clone(self.peek(arg_count as usize));
                    if !self.call_value(&call_value, arg_count) {
                        return 1;
                    }
                },
                OpCode::Closure => {
                    let closure = match self.get_current_frame_mut().read_constant() {
                        LoxValue::Closure(func) => {
                            Some(func.clone())
                        },
                        _ => None,
                    };
                    match closure {
                        Some(cl) => {
                            self.push(LoxValue::Closure(Rc::clone(&cl)));
                            for i in 0..cl.as_ref().upvalue_count {
                                let is_local = self.get_current_frame_mut().read_byte() != 0;
                                let idx = self.get_current_frame_mut().read_byte() as usize;
                                cl.upvalues.borrow_mut().push(if is_local {
                                    self.capture_upvalue(self.get_current_frame().stack_offset + idx + 1)
                                } else {
                                    self.get_current_frame().closure.upvalues.borrow()[i]
                                });
                            }
                        },
                        None => self.runtime_error("Expected function constant")
                    }
                },
                OpCode::CloseUpValue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                },
                OpCode::Return => {
                    let result = self.pop();
                    let frame = self.frames.pop().unwrap();
                    if self.frames.is_empty() {
                        self.pop();
                        return 0;
                    }
                    self.close_upvalues(frame.stack_offset);
                    self.stack.truncate(frame.stack_offset);
                    self.push_rc(result);
                },
            }
        }
    }

    fn push(&mut self, value: LoxValue) {
        self.push_rc(Rc::new(value))
    }

    fn push_rc(&mut self, value: Rc<LoxValue>) {
        if self.debug {
            println!("Pushing {}", value);
        }
        self.stack.push(value)
    }

    fn pop(&mut self) -> Rc<LoxValue> {
        let value = self.stack.pop().unwrap();
        if self.debug {
            println!("Popping {}", value);
        }
        value
    }

    fn peek(&self, distance: usize) -> &Rc<LoxValue> {
        &self.stack[self.stack.len() - distance -1]
    }

    // Returns index in the VM's UV array. idx is absolute stack slot
    fn capture_upvalue(&mut self, idx: usize) -> usize {
        if self.debug {
            println!("Capturing the following UV: {}: {} ", idx, &self.stack[idx]);
        }
        // TODO: sort by idx to speed up search
        let existing_opt = self.upvalues.iter()
            .enumerate()
            .find(|(_idx, uv)| match uv {
                UpValue::Open(existing_idx) => *existing_idx == idx,
                _ => false,
            }
            )
            .map(|(existing_idx, _)| existing_idx);

        existing_opt.unwrap_or_else(|| {
            self.upvalues.push(UpValue::Open(idx));
            self.upvalues.len() - 1
        })
    }

    fn close_upvalues(&mut self, stack_start_idx: usize) {
        for uv in self.upvalues.iter_mut() {
            match uv {
                UpValue::Open(idx) if *idx >= stack_start_idx => {
                    let stack_val = (*self.stack[*idx]).clone();
                    *uv = UpValue::Closed(RefCell::new(stack_val));
                },
                _ => (),
            }
        }
    }

    fn call_value(&mut self, callee: &Rc<LoxValue>, arg_count: u8) -> bool {
        match callee.as_ref() {
            LoxValue::Closure(func) => self.call(Rc::clone(func), arg_count),
            _ => {
                self.runtime_error("Can only call functions and classes");
                false
            },
        }
    }

    fn call(&mut self, closure: Rc<LoxClosure>, arg_count: u8) -> bool {
        if arg_count as usize != closure.function.arity {
            self.runtime_error(format!("Expected {} arguments but got {}", closure.function.arity, arg_count).as_str());
            return false;
        }
        if self.frames.len() == 255 {
            self.runtime_error("Stack overflow");
            return false;
        }
        self.frames.push(CallFrame {
            closure: Rc::clone(&closure),
            ip: 0,
            stack_offset: self.stack.len() - arg_count as usize - 1,
        });
        true
    }


    fn is_falsey(value: &LoxValue) -> bool {
        match value {
            LoxValue::Nil => true,
            LoxValue::Bool(b) => !b,
            LoxValue::Double(_) | LoxValue::String(_) | LoxValue::Closure(_) => false,
        }
    }

    fn values_equal(&self, left: &LoxValue, right: &LoxValue) -> bool {
        match (left, right) {
            (LoxValue::Bool(l), LoxValue::Bool(r)) => l == r,
            (LoxValue::Double(l), LoxValue::Double(r)) => l == r,
            (LoxValue::Nil, LoxValue::Nil) => true,
            (LoxValue::String(l), LoxValue::String(r)) => l == r,
            _ => false,
        }
    }

    fn runtime_error(&self, msg: &str) {
        println!("{}", msg);

        for frame in self.frames.iter().rev() {
            let line = frame.closure.function.chunk.line_at(frame.ip - 1);
            println!("[line {}] in {}", line, frame.closure.function.name.as_ref().unwrap_or(&"script".to_string()));
        }
    }

    fn get_current_ip(&self) -> usize {
        self.frames[self.frames.len()-1].ip
    }

    fn get_current_frame(&self) -> &CallFrame {
        let last = self.frames.len()-1;
        &self.frames[last]
    }

    fn get_current_frame_mut(&mut self) -> &mut CallFrame {
        let last = self.frames.len()-1;
        &mut self.frames[last]
    }
}