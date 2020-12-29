pub struct Ram {
    memory: Vec<u8>
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            memory: vec![0; 2048]
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        self.memory[address as usize] = byte
    }
}