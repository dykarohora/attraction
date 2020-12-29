use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;
use crate::nes::ppu::Ppu;
use crate::nes::ppu_bus::PpuBus;
use crate::nes::rom::Rom;

pub struct CpuBus {
    wram: Ram,
}

impl CpuBus {
    pub fn new() -> CpuBus {
        CpuBus {
            wram: Ram::new(2048),
        }
    }

    pub fn read_byte(&self, cartridge: &Cartridge, ppu: &Ppu, address: u16) -> u8 {
        match address {
            0x0000..=0x07FF => self.wram.read_byte(address),
            0x0800..=0x0FFF => self.wram.read_byte(address - 0x0800),
            0x1000..=0x17FF => self.wram.read_byte(address - 0x1000),
            0x1800..=0x1FFF => self.wram.read_byte(address - 0x1800),
            0x8000..=0xFFFF => cartridge.read_byte_from_program_rom(address - 0x8000),
            _ => panic!("[CPU BUS] not implemented for read_byte address:{:#06X}", address)
        }
    }

    pub fn write_byte(&mut self, cartridge: &mut Cartridge, ppu: &mut Ppu, address: u16, byte: u8) {
        match address {
            0x0000..=0x07FF => self.wram.write_byte(address, byte),
            0x0800..=0x0FFF => self.wram.write_byte(address - 0x0800, byte),
            0x1000..=0x17FF => self.wram.write_byte(address - 0x1000, byte),
            0x1800..=0x1FFF => self.wram.write_byte(address - 0x1800, byte),
            0x2000..=0x2007 => println!("[Bus] PPU I/O not implemented for write_byte address:{:#06X} byte:{:#04X}", address, byte),
            _ => panic!("[Bus] not implemented for write_byte address:{:#06X} byte:{:#04X}", address, byte)
        }
    }
}