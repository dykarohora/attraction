use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;

pub struct CpuBus {
    wram: Ram,
    cartridge: Cartridge,
}

impl CpuBus {
    pub fn new(cartridge: Cartridge) -> CpuBus {
        CpuBus {
            wram: Ram::new(),
            cartridge,
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x07FF => self.wram.read_byte(address),
            0x0800..=0x0FFF => self.wram.read_byte(address - 0x0800),
            0x1000..=0x17FF => self.wram.read_byte(address - 0x1000),
            0x1800..=0x1FFF => self.wram.read_byte(address - 0x1800),
            0x8000..=0xFFFF => self.cartridge.read_byte(address - 0x8000),
            _ => panic!("[Bus] not implemented for read_byte")
        }
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        match address {
            0x0000..=0x07FF => self.wram.write_byte(address, byte),
            0x0800..=0x0FFF => self.wram.write_byte(address - 0x0800, byte),
            0x1000..=0x17FF => self.wram.write_byte(address - 0x1000, byte),
            0x1800..=0x1FFF => self.wram.write_byte(address - 0x1800, byte),
            _ => panic!("[Bus] not implemented for write_byte")
        }
    }
}