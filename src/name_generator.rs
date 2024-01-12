pub struct NameGenerator {
    current: Vec<u8>,
}

impl NameGenerator {
    pub fn new() -> Self {
        Self {
            current: vec![b'A'],
        }
    }
}

impl Iterator for NameGenerator {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let last = *self.current.last()?;

        if self.current.len() == 1 && last < b'a' {
            if last <= b'Z' {
                *self.current.last_mut()? += 1;
                return Some(String::from_utf8(vec![last]).unwrap());
            } else {
                *self.current.last_mut()? = b'a';
            }
        }

        let ret = String::from_utf8_lossy(&self.current).to_string();
        let mut idx = self.current.len() - 1;

        if self.current[idx] != b'z' {
            self.current[idx] += 1;
        } else {
            if idx == 0 {
                self.current.insert(0, b'a' - 1);
                idx += 1;
            }

            while self.current[idx] >= b'z' {
                self.current[idx] = b'a';

                if idx != 0 {
                    self.current[idx - 1] += 1;
                    idx -= 1;
                } else {
                    self.current.insert(0, b'a');
                }
            }
        }

        Some(ret)
    }
}
