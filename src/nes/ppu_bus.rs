use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;
use std::rc::Rc;

#[derive(Default, Debug)]
pub struct PpuBus {
    vram: Ram,
    cartridge: Rc<Cartridge>,
}

impl PpuBus {
    pub fn new(cartridge: Rc<Cartridge>) -> PpuBus {
        PpuBus {
            vram: Ram::new(2048 * 2),
            cartridge,
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1FFF => self.cartridge.read_byte_from_character_rom(address),
            0x2000..=0x23FF => self.vram.read_byte(address - 0x2000),
            _ => panic!("[PPU BUS] not implemented for read_byte address:{:#06X}", address)
        }
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        match address {
            0x2000..=0x2FFF => self.vram.write_byte(address - 0x2000, byte),
            _ => panic!("[PPU BUS] not implemented for write_byte address:{:#06X}, byte:{:#06X}", address, byte)
        }
    }
}