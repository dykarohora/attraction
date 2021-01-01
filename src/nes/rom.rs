use std::cell::RefCell;

#[derive(Debug, Default)]
pub struct Rom {
    memory: RefCell<Vec<u8>>,
}

impl Rom {
    pub fn new(data: Vec<u8>) -> Rom {
        Rom {
            memory: RefCell::new(data)
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        self.memory.borrow()[address as usize]
    }
}