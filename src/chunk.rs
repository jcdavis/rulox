extern crate num;
extern crate num_derive;

use num_derive::FromPrimitive;
use num_derive::ToPrimitive;

use crate::value::LoxValue;

#[derive(Debug, FromPrimitive, PartialEq, ToPrimitive)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    CloseUpValue,
    Return,
    Class,
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<LoxValue>,
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

    pub fn write_byte(&mut self, byte: u8, line: u32) -> usize {
        self.code.push(byte);
        self.lines.push(line);
        self.code.len() - 1
    }

    pub fn patch_byte(&mut self, byte: u8, idx: usize) {
        self.code[idx] = byte;
    }

    pub fn add_constant(&mut self, constant: LoxValue) -> u8 {
        self.constants.push(constant);
        (self.constants.len() - 1) as u8
    }

    pub fn read_byte(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn read_constant(&self, offset: usize) -> &LoxValue {
        &self.constants[offset]
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

    pub fn code_len(&self) -> usize {
        self.code.len()
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
            OpCode::Constant => self.constant_instruction(as_enum, offset),
            OpCode::Nil => Self::simple_instruction(as_enum, offset),
            OpCode::True => Self::simple_instruction(as_enum, offset),
            OpCode::False => Self::simple_instruction(as_enum, offset),
            OpCode::Pop => Self::simple_instruction(as_enum, offset),
            OpCode::GetLocal => self.byte_instruction(as_enum, offset),
            OpCode::SetLocal => self.byte_instruction(as_enum, offset),
            OpCode::GetGlobal => self.constant_instruction(as_enum, offset),
            OpCode::DefineGlobal => self.constant_instruction(as_enum, offset),
            OpCode::SetGlobal => self.constant_instruction(as_enum, offset),
            OpCode::GetUpvalue => self.byte_instruction(as_enum, offset),
            OpCode::SetUpvalue => self.byte_instruction(as_enum, offset),
            OpCode::GetProperty => self.constant_instruction(as_enum, offset),
            OpCode::SetProperty => self.constant_instruction(as_enum, offset),
            OpCode::Equal => Self::simple_instruction(as_enum, offset),
            OpCode::Greater => Self::simple_instruction(as_enum, offset),
            OpCode::Less => Self::simple_instruction(as_enum, offset),
            OpCode::Add => Self::simple_instruction(as_enum, offset),
            OpCode::Subtract => Self::simple_instruction(as_enum, offset),
            OpCode::Multiply => Self::simple_instruction(as_enum, offset),
            OpCode::Divide => Self::simple_instruction(as_enum, offset),
            OpCode::Not => Self::simple_instruction(as_enum, offset),
            OpCode::Negate => Self::simple_instruction(as_enum, offset),
            OpCode::Print => Self::simple_instruction(as_enum, offset),
            OpCode::Jump => self.jump_instruction(as_enum, offset, true),
            OpCode::JumpIfFalse => self.jump_instruction(as_enum, offset, true),
            OpCode::Loop  => self.jump_instruction(as_enum, offset, false),
            OpCode::Call => self.byte_instruction(as_enum, offset),
            OpCode::Closure => {
                let constant = self.code[offset + 1];
                let mut current = offset + 2;
                let value = &self.constants[constant as usize];
                println!("{:<16} {:04} '{:?}'", format!("{:?}", as_enum), constant, value);
                match value {
                    LoxValue::Closure(func) => {
                        for _ in 0..func.upvalue_count {
                            let is_local = self.code[current];
                            let idx = self.code[current + 1];
                            let local_str = if is_local == 1 {
                                "local"
                            } else {
                                "upvalue"
                            };
                            println!("{:04}      |                     {} {}", current, local_str, idx);
                            current += 2;
                        }
                    },
                    rest => println!("WARNING: Unexpected Value {}", rest),
                }
                current
            },
            OpCode::CloseUpValue => Self::simple_instruction(as_enum, offset),
            OpCode::Return => Self::simple_instruction(as_enum, offset),
            OpCode::Class => self.constant_instruction(as_enum, offset),
        }
    }

    fn simple_instruction(instruction: OpCode, offset: usize) -> usize {
        println!("{:?}", instruction);
        offset + 1
    }

    fn byte_instruction(&self, instruction: OpCode, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:<16} {:04}", format!("{:?}", instruction), slot);
        offset + 2
    }

    fn jump_instruction(&self, instruction: OpCode, offset: usize, direction_is_pos: bool) -> usize {
        let jump = ((self.code[offset+1] as usize) << 8) + (self.code[offset+2] as usize);
        let end = if direction_is_pos {
            offset + 3 + jump
        } else {
            offset + 3 - jump
        };
        println!("{:<16} {:04} -> {:04}", format!("{:?}", instruction), offset, end);
        offset + 3
    }

    fn constant_instruction(&self, instruction: OpCode, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let value = &self.constants[constant as usize];
        println!("{:<16} {:04} '{:?}'", format!("{:?}", instruction), constant, value);
        offset + 2
    }
}