
use std::collections::HashMap;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Clone, Debug)]
pub enum LoxValue {
    Bool(bool),
    Double(f64),
    String(Rc<String>),
    Closure(Rc<LoxClosure>),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
    BoundMethod(Rc<LoxBoundMethod>),
    Nil,
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Double(d) => write!(f, "{}", d),
            LoxValue::String(s) => write!(f, "\"{}\"", *s),
            LoxValue::Closure(cl) => write!(f, "fn<{}>", cl.function.name.as_ref().unwrap_or(&"script".to_string())),
            LoxValue::Class(cl) => write!(f, "{} klass ", cl.name),
            LoxValue::Instance(inst) => write!(f, "{} instance", inst.klass.name),
            LoxValue::BoundMethod(bm) => write!(f, "fn<{}>", bm.method.function.name.as_ref().unwrap_or(&"script".to_string())),
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

#[derive(Debug)]
pub struct LoxClosure {
    pub function: LoxFunction,
    pub upvalue_count: usize,
    pub upvalues: RefCell<Vec<Rc<RefCell<UpValue>>>>,
}

#[derive(Debug)]
pub struct LoxClass {
    pub name: Rc<String>,
    pub methods: RefCell<HashMap<Rc<String>, Rc<LoxClosure>>>,
}

#[derive(Debug)]
pub struct LoxBoundMethod {
    pub receiver: Rc<LoxInstance>,
    pub method: Rc<LoxClosure>,
}

#[derive(Debug)]
pub struct LoxInstance {
    pub klass: Rc<LoxClass>,
    pub fields: RefCell<HashMap<Rc<String>, LoxValue>>,
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