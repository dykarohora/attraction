#[derive(Default, Debug)]
pub struct KeyPad {
    is_ready_latch: bool,
    current_read_index: u8,
    register: u8,
    key_buffer: u8,
}

impl KeyPad {
    pub fn update_key_buffer(&mut self, buffer: u8) {
        self.key_buffer = buffer;
    }

    pub fn read_byte(&self) -> u8 {
        let mask = 0b0000_0001 << self.current_read_index;
        (self.register & mask) >> self.current_read_index
    }

    pub fn write_byte(&mut self, byte: u8) {
        match byte {
            0x01 => {
                // ラッチ準備フラグを立てる
                self.is_ready_latch = true
            }
            0x00 => {
                // ラッチの実行
                if self.is_ready_latch {
                    self.register = self.key_buffer;
                    self.current_read_index = 0;
                }
            }
            _ => panic!("invalid operation")
        }
    }
}