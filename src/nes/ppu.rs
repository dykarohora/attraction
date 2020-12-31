use crate::nes::ppu_bus::PpuBus;
use std::cell::Cell;

pub struct Ppu {
    bus: PpuBus,
    ppu_addr: Cell<u16>,
    is_set_ppu_high_address: Cell<bool>,
}

impl Ppu {
    pub fn new(bus: PpuBus) -> Ppu {
        Ppu {
            bus,
            ppu_addr: Cell::new(0x0000),
            is_set_ppu_high_address: Cell::new(false),
        }
    }

    pub fn run(&self) {
        println!("Run ppu");
    }

    pub fn read_ppu(&self, address: u16) -> u8 {
        match address {
            0x2000 => 0,
            0x2001 => 1,
            0x2002 => 2,
            0x2003 => 3,
            0x2004 => 4,
            0x2005 => 5,
            0x2006 => 6,
            0x2007 => 7,
            _ => panic!("[Ppu] invalid address: {:#06X}", address)
        }
    }

    pub fn write_ppu(&self, address: u16, byte: u8) {
        match address {
            0x2000 => (),
            0x2001 => (),
            0x2002 => (),
            0x2003 => (),
            0x2004 => (),
            0x2005 => (),
            0x2006 => self.write_ppu_addr(byte),
            0x2007 => (),
            _ => panic!("[Ppu] invalid address: {:#06X}", address)
        };
        println!("[Ppu] Call write_ppu: address {:#06X}, byte {:#06X}", address, byte);
    }

    fn write_ppu_addr(&self, byte: u8) {
        if self.is_set_ppu_high_address.get() {
            let address = self.ppu_addr.get();
            self.ppu_addr.set((address | (byte as u16)));
            self.is_set_ppu_high_address.set(false);
            println!("[PPU] PPU address set: {:#06X}", self.ppu_addr.get());
        } else {
            self.ppu_addr.set((byte as u16) << 8);
            self.is_set_ppu_high_address.set(true);
        }
    }
}