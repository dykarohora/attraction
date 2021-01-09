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
pub struct PpuMaskRegister {
    background_color: u8,
    enable_sprite: bool,
    enable_background: bool,
    enable_sprite_mask: bool,
    enable_background_mask: bool,
    enable_monochrome_display: bool,
}

impl PpuMaskRegister {
    pub fn set_binary(&mut self, byte: u8) {
        self.background_color = match (byte & 0b1110_0000) >> 5 {
            0b000 => 0b000,
            0b001 => 0b001,
            0b010 => 0b010,
            0b100 => 0b100,
            _ => panic!("invalid operation")
        };

        self.enable_sprite = if byte & 0b0001_0000 == 0b0001_0000 { true } else { false };
        self.enable_background = if byte & 0b0000_1000 == 0b0000_1000 { true } else { false };
        self.enable_sprite_mask = if byte & 0b0000_0100 == 0b0000_0100 { true } else { false };
        self.enable_background_mask = if byte & 0b0000_0010 == 0b0000_0010 { true } else { false };
        self.enable_monochrome_display = if byte & 0b0000_0001 == 0b0000_0001 { true } else { false };
    }
}


#[derive(Default, Debug)]
pub struct Ppu {
    bus: PpuBus,
    background_palette: Vec<u8>,
    sprite_palette: Vec<u8>,
    sprite_ram: Vec<u8>,

    status_register: PpuStatusRegister,
    ctrl_register: PpuCtrlRegister,
    mask_register: PpuMaskRegister,

    ppu_addr: u16,
    sprite_ram_addr: u8,
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
            sprite_palette: vec![0; 16],
            sprite_ram: vec![0; 256],
            ppu_addr: 0x0000,
            sprite_ram_addr: 0x00,
            is_set_ppu_high_address: false,
            graphic_buffer: vec![0; 256 * 240],
            current_line: 0,
            ppu_cycle_count: 0,
            status_register: Default::default(),
            ctrl_register: Default::default(),
            mask_register: Default::default(),
        }
    }

    pub fn run(&mut self, cycle: u16) {
        self.ppu_cycle_count += cycle;

        if self.ppu_cycle_count < 341 {
            return;
        }

        self.current_line += 1;
        self.ppu_cycle_count %= 341;

        if self.mask_register.enable_background && self.current_line <= 240 && self.current_line % 8 == 0 {
            self.build_background();
        }

        if self.mask_register.enable_sprite && self.current_line == 240 {
            self.build_sprites();
            return;
        }

        if self.current_line == 241 {
            self.status_register.set_vblank();
            return;
        }

        if self.current_line == 262 {
            self.status_register.clear_vblank();
            self.current_line = 0;
            return;
        }
    }

    // 1タイルラインごとに書いていく
    fn build_background(&mut self) {
        let line_no = self.current_line / 8 - 1;
        let mut sprite = Vec::<u8>::with_capacity(16);
        let mut tile = Vec::<u8>::with_capacity(8 * 8);

        for i in 0..32 {
            // ネームテーブルからスプライト番号を取得する
            let sprite_index = self.bus.read_byte((line_no * 32 + i) + 0x2000);
            // キャラクタROMからスプライトデータを取得する

            sprite.clear();
            for j in 0..16 {
                let sprite_line = self.bus.read_byte((sprite_index as u16 * 16) + j + self.ctrl_register.base_background_pattern_address);
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

    fn build_sprites(&mut self) {
        let mut sprite = Vec::<u8>::with_capacity(16);
        let mut tile = Vec::<u8>::with_capacity(8*8);

        for i in 0..64 {
            // スプライトRAMから4バイトデータを読み出す
            let mut pos_y = self.sprite_ram[i * 4];
            if pos_y == 0 {
                continue;
            }

            pos_y += 1;

            let sprite_index = self.sprite_ram[i * 4 + 1];
            let sprite_status = self.sprite_ram[i * 4 + 2];
            let pos_x = self.sprite_ram[i * 4 + 3];

            // キャラクタROMからスプライトデータを読み取る
            sprite.clear();
            for j in 0..16 {
                let sprite_line = self.bus.read_byte((sprite_index as u16 * 16) + j + self.ctrl_register.base_sprite_pattern_address);
                sprite.push(sprite_line);
            }

            // スプライトを組み立てる
            let palette: u8 = match sprite_status & 0b0000_0011 {
                0b00 => 0,
                0b01 => 1,
                0b10 => 2,
                0b11 => 3,
                _ => panic!("invalid operation")
            };

            // 座標値に応じてグラフィックバッファにスプライトを書き込む
            tile.clear();
            for j in 0..8 {
                let sprite_low = sprite[j];
                let sprite_high = sprite[j + 8];
                // TODO ステータスを見て反転具合考えないと
                for k in (0..8).rev() {
                    let a = 0x01 << k;
                    let l = (sprite_low & a) >> k;
                    let h = ((sprite_high & a) >> k) * 2;
                    let pixel = l + h;
                    let pixel_color = self.sprite_palette[(palette * 4 + pixel) as usize];
                    tile.push(pixel_color)
                }
            }

            for row in 0..8 {
                for col in 0..8 {
                    let pos = 256 * (pos_y as usize + row) + pos_x as usize + col;
                    let pixel = tile[(row * 8 + col) as usize];
                    self.graphic_buffer[pos as usize] = COLOR[pixel as usize];
                }
            }
        }
    }

    pub fn read_ppu(&mut self, address: u16) -> u8 {
        match address {
            0x2000 => panic!("[PPU] not implemented read 0x2000"),
            0x2001 => panic!("[PPU] not implemented read 0x2001"),
            0x2002 => self.read_ppu_status(),
            0x2003 => panic!("[PPU] not implemented read 0x2002"),
            0x2004 => panic!("[PPU] not implemented read 0x2003"),
            0x2005 => panic!("[PPU] not implemented read 0x2004"),
            0x2006 => panic!("[PPU] not implemented read 0x2005"),
            0x2007 => panic!("[PPU] not implemented read 0x2006"),
            _ => panic!("[PPU] invalid address: {:#06X}", address)
        }
    }

    pub fn write_ppu(&mut self, address: u16, byte: u8) {
        match address {
            0x2000 => self.ctrl_register.set_binary(byte),
            0x2001 => self.mask_register.set_binary(byte),
            0x2002 => panic!("[PPU] not implemented write 0x2002 byte:{:#04X}", byte),
            0x2003 => self.write_sprite_addr(byte),
            0x2004 => self.write_sprite_data(byte),
            0x2005 => panic!("[PPU] not implemented write 0x2005 byte:{:#04X}", byte),
            0x2006 => self.write_ppu_addr(byte),
            0x2007 => self.write_ppu_data(byte),
            _ => panic!("[PPU] invalid address: {:#06X}", address)
        };
        // println!("[Ppu] Call write_ppu: address {:#06X}, byte {:#06X}", address, byte);
    }

    pub fn get_graphic_buffer(&self) -> &Vec<u32> {
        &self.graphic_buffer
    }

    fn write_sprite_addr(&mut self, byte: u8) {
        self.sprite_ram_addr = byte;
    }

    fn write_sprite_data(&mut self, byte: u8) {
        self.sprite_ram[self.sprite_ram_addr as usize] = byte;
        self.sprite_ram_addr += 1;
    }

    fn write_ppu_addr(&mut self, byte: u8) {
        if self.is_set_ppu_high_address {
            self.ppu_addr = self.ppu_addr | (byte as u16);
            self.is_set_ppu_high_address = false
            // println!("[PPU] PPU address set: {:#06X}", self.ppu_addr.get());
        } else {
            self.ppu_addr = (byte as u16) << 8;
            self.is_set_ppu_high_address = true
        }
    }

    fn write_ppu_data(&mut self, byte: u8) {
        // PPU ADDRによって
        match self.ppu_addr {
            0x2000..=0x23FF => self.bus.write_byte(self.ppu_addr, byte),
            0x3F00..=0x3F0F => self.write_background_palette(self.ppu_addr, byte),
            0x3F10..=0x3F1F => self.write_sprite_palette(self.ppu_addr, byte),
            _ => panic!("[Ppu] not implemented or invalid address: {:#06X} byte: {:#04X}", self.ppu_addr, byte)
        };
        // PPUDATAに書き込みが発生するとPPUADDRがインクリメントされる
        self.ppu_addr += 1;
    }

    fn read_ppu_status(&mut self) -> u8 {
        let val = self.status_register.get_binary();
        self.status_register.clear_vblank();
        val
    }

    // バックグラウンドパレットへの書き込み
    fn write_background_palette(&mut self, address: u16, byte: u8) {
        self.background_palette[(address - 0x3F00) as usize] = byte;
    }

    fn write_sprite_palette(&mut self, address: u16, byte: u8) {
        self.sprite_palette[(address - 0x3F10) as usize] = byte
    }
}