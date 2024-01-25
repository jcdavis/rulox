extern crate num;
extern crate num_derive;

use num_derive::FromPrimitive;
use num_derive::ToPrimitive;

#[derive(Debug, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<f64>,
    lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_byte(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: f64) -> u8 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u8
    }

    pub fn read_byte(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn read_constant(&self, offset: usize) -> f64 {
        self.constants[offset]
    }

    pub fn line_at(&self, offset: usize) -> u32 {
        self.lines[offset]
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset: usize = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }

    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset-1] {
            print!("   | ");
        } else {
            print!("{:04} ", self.lines[offset]);
        }
        let instruction = self.code[offset];
        let as_enum: OpCode = num::FromPrimitive::from_u8(instruction).unwrap_or_else(|| panic!("Unparseble {}", instruction));

        match as_enum {
            OpCode::Constant => self.constant_instruction(OpCode::Constant, offset),
            OpCode::Add => Self::simple_instruction(as_enum, offset),
            OpCode::Subtract => Self::simple_instruction(as_enum, offset),
            OpCode::Multiply => Self::simple_instruction(as_enum, offset),
            OpCode::Divide => Self::simple_instruction(as_enum, offset),
            OpCode::Negate => Self::simple_instruction(as_enum, offset),
            OpCode::Return => Self::simple_instruction(as_enum, offset),
        }
    }

    fn simple_instruction(instruction: OpCode, offset: usize) -> usize {
        println!("{:?}", instruction);
        offset + 1
    }

    fn constant_instruction(&self, instruction: OpCode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let value = self.constants[constant as usize];
        println!("{:<16} {:04} '{:?}'", format!("{:?}", instruction), constant, value);
        offset + 2
    }
}