extern crate num;

use std::cell::RefCell;
use std::rc::Weak;
use std::{collections::HashMap, rc::Rc};

use crate::value::{LoxBoundMethod, LoxClass, LoxInstance};
use crate::{chunk, value::{LoxClosure, LoxFunction, LoxValue, UpValue}};
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

pub struct VM {
    debug: bool,
    capture_output: bool,
    pub output: Vec<String>,
    frames: Vec<CallFrame>,
    stack: Vec<LoxValue>,
    globals: HashMap<String, LoxValue>,
    // Closure's upvalues arrays keep references to the elements in this vec.
    // We need to keep track of them to be able to dedup on creation (to avoid duplication when closing),
    // But only keep weak so when the last closure referencing an upvalue is dropped, that value itself is also dropped.
    upvalues: Vec<Weak<RefCell<UpValue>>>,
}

impl VM {
    pub fn new(script: LoxFunction, debug: bool, capture_output: bool) -> VM {
        let mut vm = VM {
            debug,
            capture_output,
            output: Vec::new(),
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
        vm.stack.push(LoxValue::Closure(closure.clone()));
        vm.call(closure, 0);
        vm
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
                            let value = self.peek(0);
                            if self.globals.contains_key(&name) {
                                self.globals.insert(name, value.clone());
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
                    let value = {
                        let slot = self.get_current_frame_mut().read_byte() as usize;
                        let uv = &self.get_current_frame().closure.upvalues.borrow()[slot];
                        let binding = (**uv).borrow();
                        match &*binding {
                            UpValue::Open(idx) => self.stack[*idx].clone(),
                            UpValue::Closed(rc) => rc.borrow().clone(),
                        }
                    };
                    self.push(value);
                },
                OpCode::SetUpvalue => {
                    // Hacky workaround beacuse I am bad at rust:
                    // We can't mutate the stack directly in the open case since we still have an immutable binding ref.
                    // So instead, return the idx to mutate and do that below with the refs all dropped.
                    let idx_opt = {
                        let slot = self.get_current_frame_mut().read_byte() as usize;
                        let uv = &self.get_current_frame().closure.upvalues.borrow()[slot];
                        let binding = (**uv).borrow();
                        match &*binding {
                            UpValue::Open(idx) => Some(*idx),
                            UpValue::Closed(rc) => {
                                *rc.borrow_mut() = self.peek(0).clone();
                                None
                            },
                        }
                    };
                    if let Some(idx) = idx_opt {
                        self.stack[idx] = self.peek(0).clone();
                    }
                },
                OpCode::GetProperty => {
                    let inst_value = self.pop();
                    if let LoxValue::Instance(inst) = inst_value {
                        let name = match self.get_current_frame_mut().read_constant() {
                            LoxValue::String(str) => Rc::clone(str),
                            _ => {
                                self.runtime_error("Expecting property name to be a string");
                                return 1;
                            },
                        };

                        let inst_clone = Rc::clone(&inst);
                        match inst_clone.fields.borrow().get(&name) {
                            Some(value) => self.push(value.clone()),
                            None => {
                                // Property not defined, try method bind
                                if !self.bind_method(inst, name) {
                                    return 1;
                                }
                            },
                        };
                    } else {
                        self.runtime_error("Can only get propertty on instances");
                        return 1;
                    }
                },
                OpCode::SetProperty => {
                    let assigned_value = self.pop();
                    let inst_value = self.pop();
                    if let LoxValue::Instance(inst) = inst_value {
                        let name = self.get_current_frame_mut().read_constant();
                        if let LoxValue::String(string) = name {
                            inst.fields.borrow_mut().insert(Rc::clone(string), assigned_value.clone());
                            self.push(assigned_value);
                        } else {
                            self.runtime_error("Expecting property name to be a string");
                            return 1
                        }
                    } else {
                        self.runtime_error("Can only set property on instances");
                        return 1;
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
                    if self.capture_output {
                        let formatted = format!("{}", self.pop());
                        self.output.push(formatted);
                    } else {
                        println!("{}", self.pop());
                    }

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
                    let call_value = self.peek(arg_count as usize).clone();
                    if !self.call_value(call_value, arg_count) {
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
                                    self.capture_upvalue(self.get_current_frame().stack_offset + idx)
                                } else {
                                    Rc::clone(&self.get_current_frame().closure.upvalues.borrow()[i])
                                });
                            }
                        },
                        None => {
                            self.runtime_error("Expected function constant");
                            return 1
                        },
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
                    self.push(result);
                },
                OpCode::Class => {
                    match self.get_current_frame_mut().read_constant() {
                        LoxValue::String(rc) => {
                            let klass = LoxClass {
                                name: Rc::clone(rc),
                                methods: RefCell::new(HashMap::new()),
                            };
                            self.push(LoxValue::Class(Rc::new(klass)));
                        }
                        _ => {
                            self.runtime_error("Expected string for class name");
                            return 1;
                        }
                    }
                },
                OpCode::Method => {
                    let name = self.get_current_frame_mut().read_constant().clone();

                    if let LoxValue::String(str) = name {
                        let method = self.peek(0);
                        let klass = self.peek(1);

                        if let LoxValue::Closure(cloj) = method {
                            if let LoxValue::Class(kl) = klass {
                                kl.methods.borrow_mut().insert(Rc::clone(&str), Rc::clone(cloj));
                                self.pop();
                            } else {
                                self.runtime_error("Expected class, got");
                                return 1;
                            }
                        } else {
                            self.runtime_error("Expected closure for method");
                            return 1;
                        }
                    } else {
                        self.runtime_error("Expected string for method name");
                        return 1;
                    }
                }
            }
        }
    }

    fn push(&mut self, value: LoxValue) {
        if self.debug {
            println!("Pushing {}", value);
        }
        self.stack.push(value)
    }

    fn pop(&mut self) -> LoxValue {
        let value = self.stack.pop().unwrap();
        if self.debug {
            println!("Popping {}", value);
        }
        value
    }

    fn peek(&self, distance: usize) -> &LoxValue {
        &self.stack[self.stack.len() - distance -1]
    }

    // idx is absolute stack slot
    fn capture_upvalue(&mut self, idx: usize) -> Rc<RefCell<UpValue>> {
        if self.debug {
            println!("Capturing the following UV: {}: {} ", idx, &self.stack[idx]);
        }
        // TODO: sort by idx to speed up search
        let existing_opt = self.upvalues.iter()
            .find_map(|weak| {
                weak.upgrade().and_then(|rc| {
                    match *(*rc).borrow() {
                        UpValue::Open(existing_idx) if existing_idx == idx => Some(Rc::clone(&rc)),
                        _ => None,
                    }
                })
            });

        existing_opt.unwrap_or_else(|| {
            let rc = Rc::new(RefCell::new(UpValue::Open(idx)));
            self.upvalues.push(Rc::downgrade(&rc));
            rc
        })
    }

    fn close_upvalues(&mut self, stack_start_idx: usize) {
        for uv in self.upvalues.iter_mut() {
            if let Some(rc) = uv.upgrade() {
                let mut binding = (*rc).borrow_mut();
                match *binding {
                    UpValue::Open(idx)if idx >= stack_start_idx => {
                        // TODO(Can we just take the value and replace with Nil?)
                        let stack_val = self.stack[idx].clone();
                        *binding = UpValue::Closed(RefCell::new(stack_val));
                    },
                    _ => (),
                }
            }
        }
    }

    fn call_value(&mut self, callee: LoxValue, arg_count: u8) -> bool {
        match callee {
            LoxValue::Closure(func) => self.call(Rc::clone(&func), arg_count),
            LoxValue::Class(cl) => {
                let inst = LoxInstance {
                    klass: Rc::clone(&cl),
                    fields: RefCell::new(HashMap::new()),
                };
                let idx = self.stack.len() - arg_count as usize - 1;
                self.stack[idx] = LoxValue::Instance(Rc::new(inst));
                true
            },
            LoxValue::BoundMethod(bm) => {
                let idx = self.stack.len() - arg_count as usize - 1;
                self.stack[idx] = LoxValue::Instance(Rc::clone(&bm.receiver));
                self.call(Rc::clone(&bm.method), arg_count);
                true
            },
            rest => {
                println!("{}", rest);
                self.runtime_error("Can only call functions and classes");
                false
            },
        }
    }

    fn bind_method(&mut self, inst: Rc<LoxInstance>, name: Rc<String>) -> bool {
        let second_inst = Rc::clone(&inst);
        let binding = second_inst.klass.methods.borrow();
        let method = binding.get(&name);
        match method {
            Some(method) => {
                let bound = LoxBoundMethod {
                    receiver: inst,
                    method: Rc::clone(method),
                };
                self.push(LoxValue::BoundMethod(Rc::new(bound)));
                true
            },
            None => {
                let msg = format!("Undefined property '{}'.", name);
                self.runtime_error(&msg);
                false
            }
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
            _ => false,
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