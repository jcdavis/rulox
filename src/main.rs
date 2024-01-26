extern crate num;

mod chunk;
mod compiler;
mod scanner;
mod vm;

use chunk::{Chunk, OpCode};
use vm::VM;
use std::{fs, io, env};

fn write_op(chunk: &mut Chunk, op: OpCode, line: u32) {
    chunk.write_byte(num::ToPrimitive::to_u8(&op).unwrap(), line)
}

fn write_constant(chunk: &mut Chunk, constant: f64, line: u32) {
    let constant = chunk.add_constant(constant);
    write_op(chunk, OpCode::Constant, line);
    chunk.write_byte(constant, line);
}

fn insert_debug_chunk(chunk: &mut Chunk) {
    write_constant(chunk, 1.2, 123);
    write_constant(chunk, 3.4, 123);
    write_op(chunk, OpCode::Add, 123);
    write_constant(chunk, 5.6, 123);
    write_op(chunk, OpCode::Divide, 123);
    write_op(chunk, OpCode::Negate, 123);
    write_op(chunk, OpCode::Return, 123);
}

fn repl(vm: &mut VM) {
    loop {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).expect("EOF");
        interpret(vm, &buf);
    }
}

fn file(vm: &mut VM, file_name: &str) {
    let contents = fs::read_to_string(file_name).unwrap_or_else(|_| {panic!("Couldn't read {}", file_name)});
    interpret(vm, &contents);
}

fn interpret(vm: &mut VM, source: &str) {
    compiler::compile(source)
}

fn main() {
    let mut chunk = Chunk::new();
    let mut vm = vm::VM::new(chunk);
    vm.debug();
    let args: Vec<String> = env::args().collect();

    match args.get(1) {
        None => repl(&mut vm),
        Some(file_name) if args.len() == 2 => file(&mut vm, file_name),
        _ => panic!("Unexpected args"),
    }

    // chunk.disassemble("test chunk");
}
