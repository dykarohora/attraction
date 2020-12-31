use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;
use std::rc::Rc;

pub struct PpuBus {
    vram: Ram,
    cartridge: Rc<Cartridge>,
}

impl PpuBus {
    pub fn new(cartridge: Rc<Cartridge>) -> PpuBus {
        PpuBus {
            vram: Ram::new(2048),
            cartridge,
        }
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        match address {
            0x2000..=0x23FF => self.vram.write_byte(address - 0x2000, byte),
            _ => panic!("[PPU BUS] not implemented for write_byte address:{:#06X}", address)
        }
    }
}