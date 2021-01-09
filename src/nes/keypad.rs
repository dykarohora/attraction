#[derive(Default, Debug)]
pub struct KeyPad {
    is_ready_latch: bool,
    input_player1: Input,
    input_player2: Input,
}

impl KeyPad {
    pub fn update_key_buffer_for_player1(&mut self, buffer: u8) {
        self.input_player1.update_key_buffer(buffer)
    }

    pub fn update_key_buffer_for_player2(&mut self, buffer: u8) {
        self.input_player2.update_key_buffer(buffer)
    }

    pub fn write_byte(&mut self, byte: u8) {
        match byte {
            0x01 => {
                self.is_ready_latch = true
            }
            0x00 => {
                if self.is_ready_latch {
                    self.is_ready_latch = false;
                    self.input_player1.latch();
                    self.input_player2.latch();
                }
            }
            _ => panic!("invalid operation")
        }
    }

    pub fn read_input(&mut self, address: u16) -> u8 {
        match address {
            0x4016 => {
                self.input_player1.read_input()
            }
            0x4017 => {
                self.input_player2.read_input()
            }
            _ => panic!("invalid operation")
        }
    }
}


#[derive(Default, Debug)]
struct Input {
    current_read_index: u8,
    register: u8,
    key_buffer: u8,
}

impl Input {
    pub fn update_key_buffer(&mut self, buffer: u8) {
        self.key_buffer = buffer
    }

    pub fn latch(&mut self) {
        self.register = self.key_buffer;
        self.current_read_index = 0;
    }

    pub fn read_input(&mut self) -> u8 {
        let mask = 0b0000_0001 << self.current_read_index;
        let val = (self.register & mask) >> self.current_read_index;
        self.current_read_index += 1;
        val
    }
}