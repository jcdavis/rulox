
use std::fmt;
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Clone, Debug)]
pub enum LoxValue {
    Bool(bool),
    Double(f64),
    String(Rc<String>),
    Function(Rc<LoxFunction>),
    Closure(Rc<LoxClosure>),
    Nil,
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Double(d) => write!(f, "{}", d),
            LoxValue::String(s) => write!(f, "\"{}\"", *s),
            LoxValue::Function(fun) => write!(f, "fn<{}>", fun.name.as_ref().unwrap_or(&"script".to_string())),
            LoxValue::Closure(cl) => write!(f, "fn<{}>", cl.function.name.as_ref().unwrap_or(&"script".to_string())),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct LoxFunction {
    pub name: Option<String>,
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Chunk,
}

#[derive(Debug)]
pub struct LoxClosure {
    pub function: LoxFunction,
}

impl LoxValue {
    pub fn as_double(&self) -> Option<f64> {
        match self {
            LoxValue::Double(d) => Some(*d),
            _ => None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            LoxValue::Bool(b) => Some(*b),
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