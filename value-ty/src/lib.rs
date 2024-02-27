use macros::float;
use rug::ops::Pow;
use rug::{Float, Integer};
use std::borrow::Cow;
use std::ops::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Type {
    Integer,
    Float,
    String,
    Bool,
    Array,
    File,
    Nil,
}

impl TryFrom<&str> for Type {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "int" => Type::Integer,
            "float" => Type::Float,
            "bool" => Type::Bool,
            "str" => Type::String,
            "array" => Type::Array,
            "file" => Type::File,
            "nil" => Type::Nil,

            _ => return Err(()),
        })
    }
}

impl Type {
    pub fn is_same_type(&self, value: &Value) -> bool {
        value.get_type() == self.get_type()
    }

    pub fn get_type(&self) -> &'static str {
        match self {
            Self::Integer => "int",
            Self::Float => "float",
            Self::String => "str",
            Self::Array => "array",
            Self::File => "file",
            Self::Bool => "bool",
            Self::Nil => "nil",
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_type())
    }
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum Value {
    Int(Integer),
    Float(Float),
    String(String),
    Bool(bool),
    Array(Vec<Value>),
    File(String),

    #[default]
    Nil,
}

impl Value {
    pub fn as_int(self) -> Integer {
        match self {
            Self::Int(i) => i,
            _ => panic!("Expected an int value, found: {}", self.get_type()),
        }
    }

    pub fn try_as_int(self) -> Option<Integer> {
        match self {
            Self::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_float(self) -> Float {
        match self.clone() {
            Self::Float(f) => f,
            Self::Int(i) => float!(i),

            _ => panic!("Expected an float value, found: {}", self.get_type()),
        }
    }

    pub fn try_as_float(self) -> Option<Float> {
        match self.clone() {
            Self::Float(f) => Some(f),
            Self::Int(i) => Some(float!(i)),

            _ => None,
        }
    }

    pub fn as_bool(self) -> bool {
        match self {
            Self::Bool(i) => i,
            _ => panic!("Expected an bool value, found: {}", self.get_type()),
        }
    }

    pub fn try_as_bool(self) -> Option<bool> {
        match self {
            Self::Bool(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_str(self) -> String {
        match self {
            Self::String(i) => i,
            _ => panic!("Expected an string value, found: {}", self.get_type()),
        }
    }

    pub fn try_as_str(self) -> Option<String> {
        match self {
            Self::String(i) => Some(i),
            _ => None,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn as_array(self) -> Vec<Value> {
        match self {
            Self::Array(arr) => arr,
            Self::String(s) => s
                .chars()
                .map(|i| Value::String(i.to_string()))
                .collect::<Vec<Value>>(),

            _ => panic!("Expected an array value, found, {}", self.get_type()),
        }
    }

    pub fn try_as_array(self) -> Option<Vec<Value>> {
        match self {
            Self::Array(arr) => Some(arr),
            Self::String(s) => Some(
                s.chars()
                    .map(|i| Value::String(i.to_string()))
                    .collect::<Vec<Value>>(),
            ),

            _ => None,
        }
    }

    pub fn try_as_file(&self) -> Option<&str> {
        match self {
            Self::File(s) | Self::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn try_cast(self, ty: Type) -> Option<Value> {
        Some(match ty {
            Type::Integer => self.try_as_int()?.into(),
            Type::Float => self.try_as_float()?.into(),
            Type::Array => self.try_as_array()?.into(),
            Type::Bool => self.try_as_bool()?.into(),
            Type::File => self.try_as_file()?.into(),

            _ => return None,
        })
    }

    pub fn binary_add(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(Integer::from(lhs + rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs + rhs))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs + rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs + rhs))),
            (Value::Array(lhs), Value::Array(rhs)) => {
                let mut arr = lhs.clone();
                arr.extend(rhs.clone());
                Some(Value::Array(arr))
            }
            (Value::Array(lhs), rhs) => {
                let mut arr = lhs.clone();
                arr.push(rhs.clone());
                Some(Value::Array(arr))
            }
            (Value::String(lhs), rhs) => Some(Value::String(format!("{}{}", lhs, rhs))),
            (lhs, Value::String(rhs)) => Some(Value::String(format!("{}{}", lhs, rhs))),
            _ => None,
        }
    }

    pub fn get_type(&self) -> &str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "str",
            Value::Bool(_) => "bool",
            Value::Array(_) => "array",
            Value::File(_) => "file",
            Value::Nil => "nil",
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Value::Int(i) => *i == 0,
            Value::Float(f) => *f == 0.0,
            _ => false,
        }
    }

    pub fn clear(&mut self) -> bool {
        match self {
            Value::String(s) => s.clear(),
            Value::Array(a) => a.clear(),
            _ => return false,
        }

        true
    }

    pub fn binary_sub(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(Integer::from(lhs - rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs - rhs))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs - rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs - rhs))),
            _ => None,
        }
    }

    pub fn binary_mul(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(Integer::from(lhs * rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs * rhs))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs * rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs * rhs))),
            (Value::String(lhs), Value::Int(rhs)) => {
                Some(Value::String(lhs.repeat(rhs.to_usize().unwrap())))
            }
            (Value::Int(lhs), Value::String(rhs)) => {
                Some(Value::String(rhs.repeat(lhs.to_usize().unwrap())))
            }
            _ => None,
        }
    }

    pub fn binary_mod(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(Integer::from(lhs % rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs % rhs))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs) % rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs % float!(rhs))),
            _ => None,
        }
    }

    pub fn binary_bitwise_xor(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(Integer::from(lhs ^ rhs))),
            _ => None,
        }
    }

    pub fn binary_pow(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs).pow(rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs.pow(rhs)))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs).pow(rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs.pow(rhs)))),
            _ => None,
        }
    }

    pub fn binary_div(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs).div(rhs))),
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs.div(rhs)))),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(float!(lhs.div(rhs)))),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(float!(lhs.div(rhs)))),
            _ => None,
        }
    }

    pub fn less_than(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs < rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs < rhs,
            (Value::Int(lhs), Value::Float(rhs)) => lhs < rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs < rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs < rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs.len() < rhs.len(),
            _ => return None,
        }))
    }

    pub fn greater_than(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs > rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs > rhs,
            (Value::Int(lhs), Value::Float(rhs)) => lhs > rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs > rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs > rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs.len() > rhs.len(),

            _ => return None,
        }))
    }

    pub fn less_than_or_equal(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs <= rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs <= rhs,
            (Value::Int(lhs), Value::Float(rhs)) => lhs <= rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs <= rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs <= rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs.len() <= rhs.len(),

            _ => return None,
        }))
    }

    pub fn greater_than_or_equal(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs >= rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs >= rhs,
            (Value::Int(lhs), Value::Float(rhs)) => lhs >= rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs >= rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs >= rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs.len() >= rhs.len(),

            _ => return None,
        }))
    }

    pub fn equal_to(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Array(lhs), Value::Array(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,

            _ => false,
        }))
    }

    pub fn not_equal_to(&self, other: &Value) -> Option<Value> {
        let Some(Value::Bool(b)) = self.equal_to(other) else {
            return None;
        };

        Some(Value::Bool(!b))
    }

    pub fn bool_eval(&self) -> bool {
        match self {
            Value::Int(i) => {
                if *i == 0 {
                    false
                } else {
                    true
                }
            }
            Value::Bool(false) | Value::Nil => false,
            Value::Float(f) if *f == 0.0 => false,
            Value::String(s) if s.is_empty() => false,

            _ => true,
        }
    }

    pub fn and(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (a, b) => a.bool_eval() && b.bool_eval(),
        }))
    }

    pub fn or(&self, other: &Value) -> Option<Value> {
        Some(Value::Bool(match (self, other) {
            (a, b) => a.bool_eval() || b.bool_eval(),
        }))
    }

    pub fn referenced_children(&self) -> Option<Vec<*mut Value>> {
        None
        // match self {
        //     Value::Array(a) => Some(a.clone()),
        //     _ => None,
        // }
    }
}

impl From<Value> for Integer {
    fn from(value: Value) -> Self {
        value.as_int()
    }
}

impl From<Value> for Float {
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
        Self::Int(value.into())
    }
}

impl From<&u32> for Value {
    fn from(value: &u32) -> Self {
        Self::Int((*value).into())
    }
}

impl From<&Integer> for Value {
    fn from(value: &Integer) -> Self {
        Value::Int(value.clone())
    }
}

impl From<Float> for Value {
    fn from(value: Float) -> Self {
        Value::Float(value)
    }
}

impl From<&Float> for Value {
    fn from(value: &Float) -> Self {
        Value::Float(value.clone())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<&bool> for Value {
    fn from(value: &bool) -> Self {
        Value::Bool(*value)
    }
}

impl From<Integer> for Value {
    fn from(value: Integer) -> Self {
        Value::Int(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::Array(value)
    }
}

impl<'a> From<Cow<'a, Vec<Value>>> for Value {
    fn from(value: Cow<Vec<Value>>) -> Self {
        Value::Array(value.into_owned())
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
                Self::Float(f) => {
                    // a temporary hack to remove trailing zeros that rug adds to floats
                    let mut s = f.to_string().trim_end_matches('0').to_owned();
                    if s.ends_with('.') {
                        s.push('0');
                    }
                    s
                }
                Self::Bool(b) => b.to_string(),
                Self::String(s) => s.to_string(),
                Self::Array(arr) => format!(
                    "[{}]",
                    arr.iter()
                        .map(|i| match i {
                            Value::String(s) => format!("\"{}\"", s),
                            _ => i.to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::File(s) => s.to_string(),
                Self::Nil => "nil".to_string(),
            }
        )
    }
}
