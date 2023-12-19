use std::ops::Div;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

impl Value {
    pub fn as_int(&self) -> i64 {
        match self {
            &Self::Int(i) => i,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            &Self::Float(f) => f,
            _ => panic!(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            &Self::Bool(i) => i,
            _ => panic!(),
        }
    }

    pub fn binary_add(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs + rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs + rhs)),
            // Concetrate strings
            (Value::String(lhs), Value::String(rhs)) => Some(Value::String(format!("{lhs}{rhs}"))),
            // Int or float
            // Float or Int
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(*lhs as f64 + *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(*lhs + *rhs as f64)),
            _ => None,
        }
    }
    pub fn get_type(&self) -> String {
        match self {
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::String(_) => "str".to_string(),
            Value::Bool(_) => "bool".to_string(),
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Int(i) => *i == 0,
            Value::Float(f) => *f == 0.0,
            _ => false,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
        }
    }
    pub fn binary_sub(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs - rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(*lhs as f64 - *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(*lhs - *rhs as f64)),
            _ => None,
        }
    }
    pub fn binary_mul(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs * rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs * rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(*lhs as f64 * *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(*lhs * *rhs as f64)),
            _ => None,
        }
    }
    pub fn binary_div(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs.div(rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs / rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(*lhs as f64 / *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(*lhs / *rhs as f64)),
            _ => None,
        }
    }

    pub fn referenced_children(&self) -> Option<Vec<*mut Value>> {
        match self {
            _ => None,
        }
    }
}

impl From<Value> for i64 {
    fn from(value: Value) -> Self {
        value.as_int()
    }
}

impl From<Value> for f64 {
    fn from(value: Value) -> Self {
        value.as_float()
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        value.as_bool()
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Int(value as _)
    }
}

impl From<&u32> for Value {
    fn from(value: &u32) -> Self {
        Self::Int(*value as _)
    }
}