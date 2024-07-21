use core::slice;
use std::{
    alloc::{alloc, dealloc, handle_alloc_error, realloc, Layout},
    fmt,
    ptr::NonNull,
};

#[derive(Debug, Clone, Copy)]
pub struct CustomStr {
    val: *mut u8,
    cap: usize,
    len: usize,
}

impl CustomStr {
    pub fn new(s: &str) -> Self {
        let len = s.len();
        let cap = len * 2;
        let layout = Layout::array::<u8>(cap).unwrap();
        let val = unsafe {
            let ptr = alloc(layout);
            std::ptr::copy_nonoverlapping(s.as_ptr(), ptr, len);
            ptr
        };
        Self { val, cap, len }
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            let slice = slice::from_raw_parts(self.val, self.len);
            std::str::from_utf8_unchecked(slice)
        }
    }

    pub fn grow(&mut self, new_cap: usize) {
        if new_cap > self.cap {
            let new_layout = Layout::array::<u8>(new_cap).unwrap();
            let new_val = unsafe {
                let ptr = realloc(self.val, new_layout, new_cap);
                if ptr.is_null() {
                    handle_alloc_error(new_layout);
                }
                ptr
            };
            self.val = new_val;
            self.cap = new_cap;
        }
    }

    pub fn shrink(&mut self, new_cap: usize) {
        if new_cap < self.cap && new_cap >= self.len {
            let new_layout = Layout::array::<u8>(new_cap).unwrap();
            let new_val = unsafe {
                let ptr = realloc(self.val, new_layout, new_cap);
                if ptr.is_null() {
                    handle_alloc_error(new_layout);
                }
                ptr
            };
            self.val = new_val;
            self.cap = new_cap;
        }
    }

    pub fn dealloc_str(&mut self) {
        let layout = Layout::array::<u8>(self.cap).unwrap();
        unsafe { dealloc(self.val, layout) };

        self.val = std::ptr::null_mut();
        self.cap = 0;
        self.len = 0;
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    String(String),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int(b) => write!(f, "{}", b),
            Constant::String(a) => write!(f, "{:?}", a.as_str()),
        }
    }
}
use crate::code::Bytecode;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Int(i64),
    Str(CustomStr),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(a) => write!(f, "{}", a),
            Value::Str(s) => write!(f, "{}", s.as_str()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct GcObject {
    pub value: NonNull<Value>,
    pub generation: u8,
    pub marked: bool,
}

pub struct VM {
    ip: usize,
    pub constants: Vec<Constant>,
    young_generation: Vec<GcObject>,
    old_generation: Vec<GcObject>,
    registers: Vec<Option<GcObject>>,
    code: Vec<u8>,
}

impl VM {
    pub fn new(code: Vec<u8>) -> Self {
        Self {
            code,
            young_generation: Vec::new(),
            old_generation: Vec::new(),
            ip: 0,
            constants: Vec::new(),
            registers: vec![None; 6],
        }
    }

    pub fn add_constant(&mut self, v: Constant) {
        self.constants.push(v);
    }

    fn allocate(&mut self, value: Value) -> GcObject {
        let layout = Layout::new::<Value>();
        let ptr = unsafe { alloc(layout) as *mut Value };
        unsafe {
            ptr.write(value);
        }
        let non_null_ptr = NonNull::new(ptr).expect("Allocation failed");
        let obj = GcObject {
            value: non_null_ptr,
            generation: 0,
            marked: false,
        };
        self.young_generation.push(obj);
        obj
    }

    pub fn run(&mut self) {
        loop {
            if self.ip >= self.code.len() {
                break;
            }

            if self.exec() {
                break;
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.code[self.ip];
        self.ip += 1;
        byte
    }

    fn minor_gc(&mut self) {
        self.mark_roots();
        self.sweep_young_generation();
    }

    fn major_gc(&mut self) {
        self.mark_roots();
        self.sweep_young_generation();
        self.sweep_old_generation();
    }

    fn mark_roots(&mut self) {
        for value in self.registers.iter_mut() {
            if value.is_some() {
                if !value.unwrap().marked {
                    value.unwrap().marked = true
                }
            }
        }
    }

    fn sweep_young_generation(&mut self) {
        let mut survivors = Vec::new();
        for obj in self.young_generation.clone().drain(..) {
            if obj.marked {
                let mut obj = obj;
                obj.marked = false;
                obj.generation += 1;
                if obj.generation > 2 {
                    self.old_generation.push(obj);
                } else {
                    survivors.push(obj);
                }
            } else {
                println!("FREED: {:?}", unsafe { *obj.value.as_ref() });
                self.free_object(obj);
            }
        }
        self.young_generation = survivors;
    }

    fn sweep_old_generation(&mut self) {
        let mut survivors = Vec::new();
        for obj in self.old_generation.clone().drain(..) {
            if obj.marked {
                let mut obj = obj;
                obj.marked = false;
                survivors.push(obj);
            } else {
                println!("FREEING OBJ OLD");
                self.free_object(obj);
            }
        }
        self.old_generation = survivors;
    }

    fn free_object(&mut self, obj: GcObject) {
        unsafe {
            match *obj.value.as_ref() {
                Value::Int(_) => {}
                Value::Str(mut p) => {
                    p.dealloc_str();
                }
            };
            let layout = Layout::new::<Value>();
            dealloc(obj.value.as_ptr() as *mut u8, layout);
        }
    }

    fn exec(&mut self) -> bool {
        let b = self.read_byte();

        const YOUNG_THRESHOLD: usize = 10;
        const OLD_THRESHOLD: usize = 10_000;

        match b {
            _ if b == Bytecode::Load as u8 => {
                let reg = self.read_byte();
                let idx = self.read_byte();
                let c = self.constants[idx as usize].clone();
                let obj = self.allocate(match c {
                    Constant::Int(i) => Value::Int(i),
                    Constant::String(s) => Value::Str(CustomStr::new(&s)),
                });
                self.registers[reg as usize] = Some(obj);
            }
            _ if b == Bytecode::Jmp as u8 => {
                let to = self.read_byte();
                self.ip = to as usize;
            }
            _ if b == Bytecode::Add as u8 => {
                let dest = self.read_byte();
                let a = self.read_byte();

                let val_a = unsafe { *self.registers[a as usize].unwrap().value.as_ref() };
                let val_b = unsafe { *self.registers[dest as usize].unwrap().value.as_ref() };

                self.registers[dest as usize] = Some(match (val_a, val_b) {
                    (Value::Int(a), Value::Int(b)) => self.allocate(Value::Int(a + b)),
                    (Value::Str(b), Value::Str(a)) => {
                        let mut a_s = a.as_str().to_string();
                        a_s.push_str(b.as_str());

                        self.allocate(Value::Str(CustomStr::new(&a_s)))
                    }
                    _ => todo!(),
                });
            }
            _ if b == Bytecode::Print as u8 => {
                let reg = self.read_byte();

                let obj = self.registers[reg as usize].clone();

                if let Some(obj) = obj {
                    let val = unsafe { *obj.value.as_ptr() };
                    print!("{}", val);
                } else {
                    panic!("Register is empty")
                }
            }
            _ if b == Bytecode::Halt as u8 => {
                if self.young_generation.len() > YOUNG_THRESHOLD {
                    self.minor_gc();
                }
                if self.old_generation.len() > OLD_THRESHOLD {
                    self.major_gc();
                }
                return true;
            }
            _ => panic!("Unknown bytecode."),
        };

        if self.young_generation.len() > YOUNG_THRESHOLD {
            self.minor_gc();
        }
        if self.old_generation.len() > OLD_THRESHOLD {
            self.major_gc();
        }

        false
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        for obj in self.young_generation.clone().drain(..) {
            self.free_object(obj);
        }
        for obj in self.old_generation.clone().drain(..) {
            self.free_object(obj);
        }
    }
}
