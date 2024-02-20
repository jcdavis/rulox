extern crate num;

use std::{rc::Rc, collections::HashMap};

use crate::{chunk, value::{LoxValue, LoxFunction}};
use chunk::{Chunk, OpCode};

struct CallFrame {
    function: Rc<LoxFunction>,
    ip: usize,
    stack_offset: usize,
}

impl CallFrame {
    fn read_constant(&mut self) -> &LoxValue {
        let new_offset = self.read_byte() as usize;
        self.function.chunk.read_constant(new_offset)
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.function.chunk.read_byte(self.ip);
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        ((self.read_byte() as u16) << 8) + (self.read_byte() as u16)
    }
}

pub struct VM {
    debug: bool,
    frames: Vec<CallFrame>,
    stack: Vec<LoxValue>,
    globals: HashMap<String, LoxValue>,
}

impl VM {
    pub fn new(script: LoxFunction) -> VM {
        let mut vm = VM {
            debug: false,
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
        };
        vm.frames.push(CallFrame {
            function: Rc::new(script),
            ip: 0,
            stack_offset: 0,
        });
        vm
    }

    pub fn debug(&mut self) {
        self.debug = true;
    }

    pub fn run(&mut self) -> u8 {
        loop {
            if self.debug {
                println!("{:?}", self.stack);
                self.get_current_frame().function.chunk.disassemble_instruction(self.get_current_ip());
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
                    let slot = self.get_current_frame_mut().read_byte() as usize + self.get_current_frame().stack_offset;

                    self.push(self.stack[slot].clone());
                },
                OpCode::SetLocal => {
                    let slot = self.get_current_frame_mut().read_byte() as usize + self.get_current_frame().stack_offset;
                    self.stack[slot] = self.peek(0).clone();
                },
                OpCode::GetGlobal => {
                    match self.get_current_frame_mut().read_constant().as_string() {
                        Some(name) => {
                            match self.globals.get(&name) {
                                Some(value) => self.push(value.clone()),
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
                            let value = self.peek(0).clone();
                            if self.globals.contains_key(&name) {
                                self.globals.insert(name, value);
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
                    match (self.peek(0).clone(), self.peek(1).clone()) {
                        (LoxValue::Double(a), LoxValue::Double(b)) => {
                            self.pop();
                            self.pop();
                            self.push(LoxValue::Double(a + b));
                        },
                        (LoxValue::String(a), LoxValue::String(b)) => {
                            self.pop();
                            self.pop();
                            let mut string = (*b).clone();
                            string.push_str(&a);
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
                    let inverted = self.is_falsey(&value);
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
                    println!("{:?}", self.pop());
                }
                OpCode::Jump => {
                    let offset = self.get_current_frame_mut().read_short();
                    self.get_current_frame_mut().ip += offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.get_current_frame_mut().read_short();
                    if self.is_falsey(self.peek(0)) {
                        self.get_current_frame_mut().ip += offset as usize;
                    }
                },
                OpCode::Loop => {
                    let offset = self.get_current_frame_mut().read_short();
                    self.get_current_frame_mut().ip -= offset as usize;
                },
                OpCode::Call => {
                    let arg_count = self.get_current_frame_mut().read_byte();
                    if !self.call_value(self.peek(arg_count as usize).clone(), arg_count) {
                        return 1;
                    }
                },
                OpCode::Return => {
                    return 0;
                },
            }
        }
    }

    fn push(&mut self, value: LoxValue) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> LoxValue {
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> &LoxValue {
        &self.stack[self.stack.len() - distance -1]
    }

    fn call_value(&mut self, callee: LoxValue, arg_count: u8) -> bool {
        match callee {
            LoxValue::Function(func) => self.call(func, arg_count),
            _ => {
                self.runtime_error("Can only call functions and classes");
                false
            },
        }
    }

    fn call(&mut self, function: Rc<LoxFunction>, arg_count: u8) -> bool {
        if arg_count as usize != function.arity {
            self.runtime_error(format!("Expected {} arguments but got {}", function.arity, arg_count).as_str());
            return false;
        }
        if self.frames.len() == 255 {
            self.runtime_error("Stack overflow");
            return false;
        }
        self.frames.push(CallFrame {
            function: Rc::clone(&function),
            ip: 0,
            stack_offset: self.stack.len() - arg_count as usize - 1,
        });
        true
    }


    fn is_falsey(&self, value: &LoxValue) -> bool {
        match value {
            LoxValue::Nil => true,
            LoxValue::Bool(b) => !b,
            LoxValue::Double(_) | LoxValue::String(_) | LoxValue::Function(_) => false,
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
            let line = frame.function.chunk.line_at(frame.ip - 1);
            println!("[line {}] in {}", line, frame.function.name.as_ref().unwrap_or(&"script".to_string()));
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