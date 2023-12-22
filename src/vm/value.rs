use std::ops::*;
use std::ptr::NonNull;

use crate::parser::Expr;

#[derive(Clone, Debug, Default)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Expr(NonNull<Expr>),

    #[default]
    Null,
}

impl Value {
    pub fn as_int(&self) -> i64 {
        match self {
            &Self::Int(i) => i,
            _ => panic!("Expected an int value, found: {}", self.get_type()),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            &Self::Float(f) => f,
            _ => panic!("Expected an float value, found: {}", self.get_type()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            &Self::Bool(i) => i,
            _ => panic!("Expected an bool value, found: {}", self.get_type()),
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::String(i) => i,
            _ => panic!("Expected an string value, found: {}", self.get_type()),
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

            _ => unreachable!(),
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

            _ => unreachable!(),
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

    pub fn less_than(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) < *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs < *rhs as f64)),

            _ => None,
        }
    }

    pub fn greater_than(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs > rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs > rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) > *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs > *rhs as f64)),

            _ => None,
        }
    }

    pub fn less_than_or_equal(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) <= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs <= *rhs as f64)),

            _ => None,
        }
    }

    pub fn greater_than_or_equal(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs >= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs >= rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) >= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs >= *rhs as f64)),

            _ => None,
        }
    }

    pub fn equal_to(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) == *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs == *rhs as f64)),

            _ => None,
        }
    }

    pub fn not_equal_to(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs != rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs != rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((*lhs as f64) != *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(*lhs != *rhs as f64)),

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

impl From<Value> for String {
    fn from(value: Value) -> Self {
        value.as_str().to_owned()
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(value: &'a str) -> Self {
        Value::String(value.to_owned())
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

impl<'a> Add for &'a Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        self.binary_add(rhs).unwrap()
    }
}

impl<'a> Sub for &'a Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        self.binary_sub(rhs).unwrap()
    }
}

impl<'a> Mul for &'a Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        self.binary_mul(rhs).unwrap()
    }
}

impl<'a> Div for &'a Value {
    type Output = Value;
    fn div(self, rhs: Self) -> Self::Output {
        self.binary_div(rhs).unwrap()
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int(i) => i.to_string(),
                Self::Float(f) => f.to_string(),
                Self::Bool(b) => b.to_string(),
                Self::String(s) => s.to_string(),

                _ => unreachable!(),
            }
        )
    }
}
