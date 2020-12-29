use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;

pub struct PpuBus {
    vram: Ram,
    cartridge: Cartridge
}

impl PpuBus {
    pub fn new(cartridge: Cartridge) -> PpuBus {
        PpuBus {
            vram: Ram::new(2048),
            cartridge
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            _ => panic!("[PPU BUS] Not implemented for read_byte")
        }
    }

    pub fn write_byte(&self, address: u16, byte: u8) {
        match address {
            _ => panic!("[PPU BUS] Not implemented for write_byte")
        }
    }
}