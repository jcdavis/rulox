extern crate num;

use std::{rc::Rc, collections::HashMap};

use crate::{chunk, value::LoxValue};
use chunk::{Chunk, OpCode};

pub struct VM<'a> {
    chunk: &'a Chunk,
    offset: usize,
    debug: bool,
    stack: Vec<LoxValue>,
    globals: HashMap<String, LoxValue>,
}

impl VM<'_> {
    pub fn new(chunk: &Chunk) -> VM {
        VM {
            chunk,
            offset: 0,
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn debug(&mut self) {
        self.debug = true;
    }

    pub fn run(&mut self) -> u8 {
        loop {
            if self.debug {
                println!("{:?}", self.stack);
                self.chunk.disassemble_instruction(self.offset);
            }
            let inst: OpCode = num::FromPrimitive::from_u8(self.read_byte()).unwrap();
            match inst {
                OpCode::Constant => {
                    let constant = self.read_constant().clone();
                    self.push(constant);
                },
                OpCode::Nil => self.push(LoxValue::Nil),
                OpCode::True => self.push(LoxValue::Bool(true)),
                OpCode::False => self.push(LoxValue::Bool(false)),
                OpCode::Pop => { self.pop(); },
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.push(self.stack[slot as usize].clone());
                },
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.peek(0).clone();
                },
                OpCode::GetGlobal => {
                    match self.read_constant().as_string() {
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
                    match self.read_constant().as_string() {
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
                    match self.read_constant().as_string() {
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

    fn is_falsey(&self, value: &LoxValue) -> bool {
        match value {
            LoxValue::Nil => true,
            LoxValue::Bool(b) => !b,
            LoxValue::Double(_) | LoxValue::String(_) => false,
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

    fn read_constant(&mut self) -> &LoxValue {
        let new_offset = self.read_byte() as usize;
        self.chunk.read_constant(new_offset)
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.offset);
        self.offset += 1;
        byte
    }

    fn runtime_error(&self, msg: &str) {
        println!("{}", msg);
        let line = self.chunk.line_at(self.offset - 1);
        println!("[line {}] in script", line);
    }
}