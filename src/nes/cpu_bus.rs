use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;

struct Bus {
    wram: Ram,
    cartridge: Cartridge
}

impl Bus {
    pub fn new(mut cartridge: Cartridge) -> Bus {
        Bus {
            wram: Ram::new(),
            cartridge
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x07FF => self.wram.read_byte(address),
            0x0800..=0x0FFF => self.wram.read_byte(address - 0x0800),
            0x1000..=0x17FF => self.wram.read_byte(address - 0x1000),
            0x1800..=0x1FFF => self.wram.read_byte(address - 0x1800),
            0x8000..=0xFFFF => self.cartridge.read_byte(address - 0x8000),
            _ => panic!("[Bus] not implemented")
        }
    }
}