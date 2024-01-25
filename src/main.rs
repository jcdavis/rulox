extern crate num;

mod chunk;
mod vm;

use chunk::{Chunk, OpCode};

fn write_op(chunk: &mut Chunk, op: OpCode, line: u32) {
    chunk.write_byte(num::ToPrimitive::to_u8(&op).unwrap(), line)
}

fn write_constant(chunk: &mut Chunk, constant: f64, line: u32) {
    let constant = chunk.add_constant(constant);
    write_op(chunk, OpCode::Constant, line);
    chunk.write_byte(constant, line);
}

fn main() {
    let mut chunk = Chunk::new();

    write_constant(&mut chunk, 1.2, 123);
    write_constant(&mut chunk, 3.4, 123);
    write_op(&mut chunk, OpCode::Add, 123);
    write_constant(&mut chunk, 5.6, 123);
    write_op(&mut chunk, OpCode::Divide, 123);
    write_op(&mut chunk, OpCode::Negate, 123);
    write_op(&mut chunk, OpCode::Return, 123);

    // chunk.disassemble("test chunk");
    let mut vm = vm::VM::new(chunk);
    vm.debug();
    vm.run();
}
