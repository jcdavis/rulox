extern crate num;

use crate::chunk;
use chunk::{Chunk, OpCode};

pub struct VM {
    chunk: Chunk,
    offset: usize,
    debug: bool,
    stack: Vec<f64>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            offset: 0,
            debug: false,
            stack: Vec::new(),
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
                    let constant = self.read_constant();
                    self.push(constant);
                },
                OpCode::Add => {
                    let total = self.pop() + self.pop();
                    self.push(total);
                }
                OpCode::Subtract => {
                    let total = self.pop() - self.pop();
                    self.push(total);
                }
                OpCode::Multiply => {
                    let total = self.pop() * self.pop();
                    self.push(total);
                }
                OpCode::Divide => {
                    let total = self.pop() / self.pop();
                    self.push(total);
                }
                OpCode::Negate => {
                    let negated = -self.pop();
                    self.push(negated);
                }
                OpCode::Return => {
                    println!("{:?}", self.pop());
                    return 0;
                },
            }
        }
    }

    fn push(&mut self, value: f64) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> f64 {
        self.stack.pop().unwrap()
    }

    fn read_constant(&mut self) -> f64 {
        let new_offset = self.read_byte() as usize;
        self.chunk.read_constant(new_offset)
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.read_byte(self.offset);
        self.offset += 1;
        byte
    }
}