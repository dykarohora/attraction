#[derive(Debug, Default)]
pub struct PpuStatusRegister {
    is_vblank: bool,
    is_sprite_hit: bool,
    scan_sprite: bool,
}

impl PpuStatusRegister {
    pub fn set_vblank(&mut self) {
        self.is_vblank = true
    }

    pub fn clear_vblank(&mut self) {
        self.is_vblank = false
    }

    pub fn set_sprite_hit(&mut self) {
        self.is_sprite_hit = true
    }

    pub fn clear_sprite_hit(&mut self) {
        self.is_sprite_hit = false
    }

    pub fn set_scan_sprite(&mut self) {
        self.scan_sprite = true
    }

    pub fn clear_scan_sprite(&mut self) {
        self.scan_sprite = false
    }

    pub fn get_binary(&self) -> u8 {
        let mut binary = 0b0000_0000;
        binary = binary | if self.is_vblank == true { 0b1000_0000 } else { 0b0000_0000 };
        binary = binary | if self.is_sprite_hit == true { 0b0100_0000 } else { 0b0000_0000 };
        binary = binary | if self.scan_sprite == true { 0b0010_0000 } else { 0b0000_0000 };
        binary
    }
}