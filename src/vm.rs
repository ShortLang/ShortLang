use std::{
    alloc::{alloc, dealloc, Layout},
    ptr::NonNull,
};

#[derive(Debug, Clone, Copy)]
pub struct CustomStr {
    val: *mut str,
    cap: usize,
    len: usize,
}

impl CustomStr {
    pub fn new(s: &str) -> Self {
        Self {
            val: Box::leak(s.to_string().into_boxed_str()),
            cap: s.len() * 2,
            len: s.len(),
        }
    }

    // TODO: implement string grow and shrink when update - realloc

    pub fn dealloc_str(&self) {
        let l = Layout::new::<String>();
        unsafe { dealloc(self.val as *mut u8, l) };
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    I64(i64),
    String(String),
}
use crate::code::Bytecode;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    I64(i64),
    Str(CustomStr),
}

#[derive(Debug, Clone, Copy)]
struct GcObject {
    pub value: Value,
    pub generation: u8,
    pub marked: bool,
}

pub struct VM {
    ip: usize,
    constants: Vec<Constant>,
    young_generation: Vec<NonNull<GcObject>>,
    old_generation: Vec<NonNull<GcObject>>,
    registers: Vec<*mut GcObject>,
    code: Vec<u8>,
}

impl VM {
    pub fn new(code: Vec<u8>) -> Self {
        return Self {
            code,
            young_generation: Vec::new(),
            old_generation: Vec::new(),
            ip: 0,
            constants: Vec::new(),
            registers: vec![std::ptr::null_mut(); 4],
        };
    }
    pub fn add_constant(&mut self, v: Constant) {
        self.constants.push(v);
    }
    fn allocate(&mut self, value: Value) -> NonNull<GcObject> {
        let layout = Layout::new::<GcObject>();
        let ptr = unsafe { alloc(layout) as *mut GcObject };
        let obj = GcObject {
            value,
            generation: 0,
            marked: false,
        };
        unsafe {
            ptr.write(obj);
        }
        let non_null_ptr = NonNull::new(ptr).expect("Allocation failed");
        self.young_generation.push(non_null_ptr);
        non_null_ptr
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
        println!("{:?}", self.registers);
    }
    fn read_byte(&mut self) -> u8 {
        let byte = self.code[self.ip.clone()];
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
        for value in self.registers.clone() {
            self.mark_value(value)
        }
    }

    fn mark_value(&mut self, ptr: *mut GcObject) {
        if ptr.is_null() {
            return;
        }
        unsafe {
            if !(*ptr).marked {
                (*ptr).marked = true;
                // array stuff.
            }
        }
    }
    fn sweep_young_generation(&mut self) {
        let mut survivors = Vec::new();
        for &obj_ptr in &self.young_generation.clone() {
            unsafe {
                if (*obj_ptr.as_ptr()).marked {
                    (*obj_ptr.as_ptr()).marked = false;
                    (*obj_ptr.as_ptr()).generation += 1;
                    if (*obj_ptr.as_ptr()).generation > 2 {
                        self.old_generation.push(obj_ptr);
                    } else {
                        survivors.push(obj_ptr);
                    }
                } else {
                    println!("FREED: {:?}", *(obj_ptr.as_ptr()));
                    self.free_object(obj_ptr);
                }
            }
        }
        self.young_generation = survivors;
    }
    fn sweep_old_generation(&mut self) {
        let mut survivors = Vec::new();
        for &obj_ptr in &self.old_generation.clone() {
            unsafe {
                if (*obj_ptr.as_ptr()).marked {
                    (*obj_ptr.as_ptr()).marked = false;
                    survivors.push(obj_ptr);
                } else {
                    println!("FREEING OBJ OLD");
                    self.free_object(obj_ptr);
                }
            }
        }
        self.old_generation = survivors;
    }

    fn free_object(&mut self, obj_ptr: NonNull<GcObject>) {
        unsafe {
            match (*obj_ptr.as_ptr()).value {
                Value::I64(_) => {}
                Value::Str(p) => {
                    p.dealloc_str();
                }
            };
            let layout = Layout::new::<GcObject>();
            dealloc(obj_ptr.as_ptr() as *mut u8, layout);
        }
    }
    fn exec(&mut self) -> bool {
        let b = self.read_byte();

        const YOUNG_THRESOLD: usize = 0;
        const OLD_THRESOLD: usize = 10_000;

        match b {
            _ if b == Bytecode::Mov as u8 => {
                let reg = self.read_byte();
                let idx = self.read_byte();
                let c = self.constants[idx as usize].clone();
                let p = self.allocate(match c {
                    Constant::I64(i) => Value::I64(i),
                    Constant::String(s) => {
                        let s = s.into_boxed_str();
                        Value::Str(CustomStr {
                            cap: s.len() * 2,
                            len: s.len(),
                            val: Box::leak(s),
                        })
                    }
                });
                self.registers[reg as usize] = p.as_ptr();
            }
            _ if b == Bytecode::Jmp as u8 => {
                let to = self.read_byte();

                self.ip = to as usize;
            }
            _ if b == Bytecode::Add as u8 => {
                let a = self.read_byte();
                let dest = self.read_byte();

                let val_a = unsafe { *self.registers[a as usize] }.value;
                let val_b = unsafe { *self.registers[dest as usize] }.value;

                self.registers[dest as usize] = match (val_a, val_b) {
                    (Value::I64(a), Value::I64(b)) => self.allocate(Value::I64(a + b)).as_ptr(),
                    _ => todo!(),
                }
            }
            _ if b == Bytecode::Halt as u8 => {
                if self.young_generation.len() > YOUNG_THRESOLD {
                    self.minor_gc();
                }
                if self.old_generation.len() > OLD_THRESOLD {
                    self.major_gc();
                }

                return true;
            }
            _ => panic!("Unknown bytecode."),
        };

        if self.young_generation.len() > YOUNG_THRESOLD {
            self.minor_gc();
        }
        if self.old_generation.len() > OLD_THRESOLD {
            self.major_gc();
        }

        return false;
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        println!("DROPPING VM");
        for obj_ptr in self.young_generation.clone() {
            self.free_object(obj_ptr);
        }
        for obj_ptr in self.old_generation.clone() {
            self.free_object(obj_ptr);
        }
    }
}
