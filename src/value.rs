
use std::fmt;
use std::cell::RefCell;
use std::process::id;
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Clone, Debug)]
pub enum LoxValue {
    Bool(bool),
    Double(f64),
    String(Rc<String>),
    Closure(Rc<LoxClosure>),
    Nil,
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Double(d) => write!(f, "{}", d),
            LoxValue::String(s) => write!(f, "\"{}\"", *s),
            LoxValue::Closure(cl) => write!(f, "fn<{}>", cl.function.name.as_ref().unwrap_or(&"script".to_string())),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct LoxFunction {
    pub name: Option<String>,
    pub arity: usize,
    pub chunk: Chunk,
}

#[derive(Debug)]
pub enum UpValue {
    Open(usize),
    Closed(RefCell<LoxValue>),
}

impl Drop for UpValue {
    fn drop(&mut self) {
        match self {
            UpValue::Open(idx) => println!("How is an open UV being dropped??? {}", idx),
            UpValue::Closed(rc) => println!("dropping {:?}", rc),
        }
    }
}

#[derive(Debug)]
pub struct LoxClosure {
    pub function: LoxFunction,
    pub upvalue_count: usize,
    pub upvalues: RefCell<Vec<Rc<RefCell<UpValue>>>>,
}

impl LoxValue {
    pub fn as_double(&self) -> Option<f64> {
        match self {
            LoxValue::Double(d) => Some(*d),
            _ => None
        }
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            LoxValue::String(contents) => Some((**contents).clone()),
            _ => None,
        }
    }
}