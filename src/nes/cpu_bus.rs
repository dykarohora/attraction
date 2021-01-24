use crate::nes::ram::Ram;
use crate::nes::cartridge::Cartridge;
use std::rc::Rc;
use crate::nes::ppu::Ppu;
use std::cell::RefCell;
use crate::nes::keypad::KeyPad;

#[derive(Default, Debug)]
pub struct CpuBus {
    wram: Ram,
    ppu: Rc<RefCell<Ppu>>,
    cartridge: Rc<Cartridge>,
    keypad: Rc<RefCell<KeyPad>>,
}

impl CpuBus {
    pub fn new(ppu: Rc<RefCell<Ppu>>, cartridge: Rc<Cartridge>, keypad: Rc<RefCell<KeyPad>>) -> CpuBus {
        CpuBus {
            wram: Ram::new(2048),
            ppu,
            cartridge,
            keypad,
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x07FF => self.wram.read_byte(address),
            0x0800..=0x0FFF => self.wram.read_byte(address - 0x0800),
            0x1000..=0x17FF => self.wram.read_byte(address - 0x1000),
            0x1800..=0x1FFF => self.wram.read_byte(address - 0x1800),
            0x2000..=0x2007 => self.ppu.borrow_mut().read_ppu(address),
            0x4016 | 0x4017 => self.keypad.borrow_mut().read_input(address),
            0x8000..=0xFFFF => self.cartridge.read_byte(address - 0x8000),
            _ => panic!("[Bus] not implemented for read_byte address:{:#06X}", address)
        }
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        match address {
            0x0000..=0x07FF => self.wram.write_byte(address, byte),
            0x0800..=0x0FFF => self.wram.write_byte(address - 0x0800, byte),
            0x1000..=0x17FF => self.wram.write_byte(address - 0x1000, byte),
            0x1800..=0x1FFF => self.wram.write_byte(address - 0x1800, byte),
            0x2000..=0x2007 => self.ppu.borrow_mut().write_ppu(address, byte),
            0x4000..=0x4015 => {}
            0x4016 => self.keypad.borrow_mut().write_byte(byte),
            0x4017 => {}
            _ => panic!("[Bus] not implemented for write_byte address:{:#06X} byte:{:#04X}", address, byte)
        }
    }
}