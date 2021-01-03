use crate::nes::ppu_bus::PpuBus;
use std::cell::{Cell, RefCell, Ref};
use crate::nes::color::COLOR;
use crate::nes::ppu_status_register::PpuStatusRegister;

#[derive(Debug, Default)]
pub struct PpuCtrlRegister {
    is_generate_nmi_when_vblank: bool,
    is_slave: bool,
    is_high_sprite: bool,
    base_background_pattern_address: u16,
    base_sprite_pattern_address: u16,
    ppu_address_inc_size: u8,
    base_name_table_address: u16,
}

impl PpuCtrlRegister {
    pub fn set_binary(&mut self, byte: u8) {
        self.is_generate_nmi_when_vblank = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
        self.is_slave = if byte & 0b0100_0000 == 0b0100_0000 { true } else { false };
        self.is_high_sprite = if byte & 0b0010_0000 == 0b0010_0000 { true } else { false };

        self.base_background_pattern_address = if byte & 0b0001_0000 == 0b0001_0000 { 0x1000 } else { 0x0000 };
        self.base_sprite_pattern_address = if byte & 0b0000_1000 == 0b0000_1000 { 0x1000 } else { 0x0000 };
        self.ppu_address_inc_size = if byte & 0b0000_0100 == 0b0000_0100 { 32 } else { 1 };
        self.is_generate_nmi_when_vblank = if byte & 0b0000_0011 == 0b0000_0010 { true } else { false };

        self.base_name_table_address = match byte & 0b0000_0011 {
            0b00 => { 0x2000 }
            0b01 => { 0x2400 }
            0b10 => { 0x2800 }
            0b11 => { 0x2C00 }
            _ => panic!("invalid operation")
        }
    }
}

#[derive(Default, Debug)]
pub struct Ppu {
    bus: PpuBus,
    background_palette: Vec<u8>,

    status_register: PpuStatusRegister,
    ctrl_register: PpuCtrlRegister,

    ppu_addr: u16,
    is_set_ppu_high_address: bool,
    current_line: u16,
    ppu_cycle_count: u16,

    graphic_buffer: Vec<u32>,
}

impl Ppu {
    pub fn new(ppu_bus: PpuBus) -> Ppu {
        Ppu {
            bus: ppu_bus,
            background_palette: vec![0; 16],
            ppu_addr: 0x0000,
            is_set_ppu_high_address: false,
            graphic_buffer: vec![0; 256 * 240],
            current_line: 0,
            ppu_cycle_count: 0,
            status_register: Default::default(),
            ctrl_register: Default::default(),
        }
    }

    pub fn run(&mut self, cycle: u16) {
        self.ppu_cycle_count += cycle;

        if self.ppu_cycle_count >= 341 {
            // self.current_line.set(self.current_line.get() + 1);
            self.current_line += 1;
            // self.ppu_cycle_count.set(self.ppu_cycle_count.get() % 341);
            self.ppu_cycle_count %= 341;

            // let current_line = self.current_line.get();

            if self.current_line <= 240 && self.current_line % 8 == 0 {
                self.build_background();
            }

            if self.current_line == 241 {
                self.status_register.set_vblank();
            }

            if self.current_line == 262 {
                // self.current_line.set(0);
                self.status_register.clear_vblank();
                self.current_line = 0
            }
        }
    }

    // 1タイルラインごとに書いていく
    fn build_background(&mut self) {
        let line_no = self.current_line / 8 - 1;
        let mut sprite = Vec::<u8>::with_capacity(16);
        let mut tile = Vec::<u8>::with_capacity(8 * 8);

        for i in 0..32 {
            // ネームテーブルからスプライト番号を取得する
            let sprite_num = self.bus.read_byte((line_no * 32 + i) + 0x2000);
            // キャラクタROMからスプライトデータを取得する

            sprite.clear();
            for j in 0..16 {
                let sprite_line = self.bus.read_byte((sprite_num as u16 * 16) + j);
                sprite.push(sprite_line);
            }

            // 属性テーブルからパレットを取り出す
            // このタイルの属性テーブルの場所
            // TODO ここ結構無駄
            let attribute_pos = (line_no / 4) * 8 + (i / 4);
            // 属性テーブルからアトリビュート取り出す
            let attribute = self.bus.read_byte(attribute_pos + 0x23C0);

            // タイルがブロックのうち、どこに該当するかを調べる
            let block_id = match line_no % 4 {
                0 | 1 => {
                    match i % 4 {
                        0 | 1 => 0,
                        2 | 3 => 1,
                        _ => panic!("invalid operation")
                    }
                }
                2 | 3 => {
                    match i % 4 {
                        0 | 1 => 2,
                        2 | 3 => 3,
                        _ => panic!("invalid operation")
                    }
                }
                _ => panic!("invalid operation")
            };

            // タイルのパレット番号を特定する
            let palette = match block_id {
                0 => (attribute & 0xC0) >> 6,
                1 => (attribute & 0x30) >> 4,
                2 => (attribute & 0x0C) >> 2,
                3 => (attribute & 0x03),
                _ => panic!("invalid operation")
            };

            // タイルピクセルに色情報をセットしていく
            tile.clear();
            for j in 0..8 {
                let sprite_low = sprite[j];
                let sprite_high = sprite[j + 8];
                for k in (0..8).rev() {
                    let a = 0x01 << k;
                    let l = (sprite_low & a) >> k;
                    let h = ((sprite_high & a) >> k) * 2;
                    let pixel = l + h;
                    let pixel_color = self.background_palette[(palette * 4 + pixel) as usize];
                    tile.push(pixel_color);
                }
            }

            for row in 0..8 {
                for col in 0..8 {
                    let pos = line_no * 2048 + i * 8 + row * 256 + col;
                    let pixel = tile[(row * 8 + col) as usize];
                    self.graphic_buffer[pos as usize] = COLOR[pixel as usize];
                }
            }
        }
    }

    pub fn read_ppu(&self, address: u16) -> u8 {
        match address {
            0x2000 => 0,
            0x2001 => 1,
            0x2002 => self.read_ppu_status(),
            0x2003 => 3,
            0x2004 => 4,
            0x2005 => 5,
            0x2006 => 6,
            0x2007 => 7,
            _ => panic!("[Ppu] invalid address: {:#06X}", address)
        }
    }

    pub fn write_ppu(&mut self, address: u16, byte: u8) {
        match address {
            0x2000 => self.ctrl_register.set_binary(byte),
            0x2001 => (),
            0x2002 => (),
            0x2003 => (),
            0x2004 => (),
            0x2005 => (),
            0x2006 => self.write_ppu_addr(byte),
            0x2007 => self.write_ppu_data(byte),
            _ => panic!("[Ppu] invalid address: {:#06X}", address)
        };
        // println!("[Ppu] Call write_ppu: address {:#06X}, byte {:#06X}", address, byte);
    }

    pub fn get_graphic_buffer(&self) -> &Vec<u32> {
        &self.graphic_buffer
    }

    fn write_ppu_addr(&mut self, byte: u8) {
        if self.is_set_ppu_high_address {
            // let address = self.ppu_addr.get();
            // self.ppu_addr.set((address | (byte as u16)));
            self.ppu_addr = self.ppu_addr | (byte as u16);

            // self.is_set_ppu_high_address.set(false);
            self.is_set_ppu_high_address = false
            // println!("[PPU] PPU address set: {:#06X}", self.ppu_addr.get());
        } else {
            // self.ppu_addr.set((byte as u16) << 8);
            self.ppu_addr = (byte as u16) << 8;
            // self.is_set_ppu_high_address.set(true);
            self.is_set_ppu_high_address = true
        }
    }

    fn write_ppu_data(&mut self, byte: u8) {
        // PPU ADDRによって
        // let ppu_address = self.ppu_addr.get();
        match self.ppu_addr {
            0x2000..=0x23FF => self.bus.write_byte(self.ppu_addr, byte),
            0x3F00..=0x3F0F => self.write_background_palette(self.ppu_addr, byte),
            _ => panic!("[Ppu] not implemented or invalid address: {:#06X} byte: {:#04X}", self.ppu_addr, byte)
        }
        // PPUDATAに書き込みが発生するとPPUADDRがインクリメントされる
        // self.ppu_addr.set(ppu_address + 1);
        self.ppu_addr += 1;
    }

    fn read_ppu_status(&self) -> u8 {
        self.status_register.get_binary()
    }

    // バックグラウンドパレットへの書き込み
    fn write_background_palette(&mut self, address: u16, byte: u8) {
        self.background_palette[(address - 0x3F00) as usize] = byte;
    }
}