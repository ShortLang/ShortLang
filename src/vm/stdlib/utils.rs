

use az::SaturatingCast;
use rug::Integer;

use super::Value;

pub fn convert_to_i128(value: &Value) -> Result<i128, String> {
    match value {
        Value::Int(i) => Ok(i.saturating_cast()),
        Value::Float(f) => Ok(f
            .to_integer()
            .unwrap_or_else(|| {
                // Only way for the unwrap to fail is if the float is infinity
                if f.is_sign_negative() {
                    Integer::from(i128::MIN)
                } else {
                    Integer::from(i128::MAX)
                }
            })
            .saturating_cast()),

        _ => Err("Expected a number".to_owned()),
    }
}
