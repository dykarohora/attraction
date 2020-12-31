use crate::nes::ppu_bus::PpuBus;
use std::cell::{Cell, RefCell};

pub struct Ppu {
    bus: RefCell<PpuBus>,
    background_palette: RefCell<Vec<u8>>,
    ppu_addr: Cell<u16>,
    is_set_ppu_high_address: Cell<bool>,
    graphic_buffer: RefCell<Vec<u32>>,
    current_line: Cell<u16>,
    ppu_cycle_count: Cell<u16>,
}

impl Ppu {
    pub fn new(ppu_bus: PpuBus) -> Ppu {
        Ppu {
            bus: RefCell::new(ppu_bus),
            background_palette: RefCell::new(vec![0; 16]),
            ppu_addr: Cell::new(0x0000),
            is_set_ppu_high_address: Cell::new(false),
            graphic_buffer: RefCell::new(vec![0; 256 * 240]),
            current_line: Cell::new(0),
            ppu_cycle_count: Cell::new(0),
        }
    }

    pub fn run(&self, cycle: u16) {
        self.ppu_cycle_count.set(self.ppu_cycle_count.get() + cycle);

        if self.ppu_cycle_count.get() >= 341 {
            self.current_line.set(self.current_line.get() + 1);
            self.ppu_cycle_count.set(self.ppu_cycle_count.get() % 341);

            let current_line = self.current_line.get();

            if current_line <= 240 && current_line % 8 == 0 {
                let line_no = current_line/8;
                println!("Draw background #{}", line_no);
            }

            if current_line == 241 {
                println!("Set Vblank");
            }

            if current_line == 262 {
                println!("Completed draw frame");
                self.current_line.set(0);
            }
        }
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
            0x2007 => self.write_ppu_data(byte),
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

    fn write_ppu_data(&self, byte: u8) {
        // PPU ADDRによって
        let ppu_address = self.ppu_addr.get();
        match ppu_address {
            0x2000..=0x23FF => self.bus.borrow_mut().write_byte(ppu_address, byte),
            0x3F00..=0x3F0F => self.write_background_palette(ppu_address, byte),
            _ => panic!("[Ppu] not implemented or invalid address: {:#06X} byte: {:#04X}", ppu_address, byte)
        }
        // PPUDATAに書き込みが発生するとPPUADDRがインクリメントされる
        self.ppu_addr.set(ppu_address + 1);
    }

    // バックグラウンドパレットへの書き込み
    fn write_background_palette(&self, address: u16, byte: u8) {
        self.background_palette.borrow_mut()[(address - 0x3F00) as usize] = byte;
    }
}