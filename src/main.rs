extern crate num;

mod chunk;
mod compiler;
mod value;
mod scanner;
mod vm;

use compiler::compile;

use vm::VM;
use std::{fs, io, env};

fn repl() {
    loop {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).expect("EOF");
        interpret(buf.as_str());
    }
}

fn file(file_name: &str) {
    let contents = fs::read_to_string(file_name).unwrap_or_else(|_| {panic!("Couldn't read {}", file_name)});
    interpret(contents.as_str());
}

fn interpret(source: &str) {
    let chunk = compile(source).expect("error parsing");
    let mut vm = VM::new(&chunk);
    vm.run();
}


fn main() {
    let args: Vec<String> = env::args().collect();

    match args.get(1) {
        None => repl(),
        Some(file_name) if args.len() == 2 => file( file_name),
        _ => panic!("Unexpected args"),
    }

    // chunk.disassemble("test chunk");
}
