use crate::nes::ppu_bus::PpuBus;
use crate::nes::cartridge::Cartridge;

pub struct Ppu {
    ppu_ctrl: u8,
    ppu_mask: u8,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            ppu_ctrl: 0,
            ppu_mask: 0,
        }
    }

    pub fn run(&mut self, cartridge: &Cartridge) {
        println!("run ppu");
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        // match address {
        //     0x2000 => self.ppu_ctrl = byte,
        //     0x2001 => self.ppu_mask = byte,
        //     0x2005 => {}
        //     0x2006 => {}
        //     0x2007 => {}
        //     _ => panic!("[PPU] Not implemented for write_byte address:{:#06X} byte:{:#04X}", address, byte)
        // }
    }
}