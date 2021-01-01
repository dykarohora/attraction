use crate::nes::ppu_bus::PpuBus;
use std::cell::{Cell, RefCell, Ref};

pub struct Ppu {
    bus: RefCell<PpuBus>,
    background_palette: RefCell<Vec<u8>>,
    ppu_addr: Cell<u16>,
    is_set_ppu_high_address: Cell<bool>,
    graphic_buffer: RefCell<Vec<u8>>,
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
                self.build_background();
            }

            if current_line == 241 {}

            if current_line == 262 {
                self.current_line.set(0);
            }
        }
    }

    // 1タイルラインごとに書いていく
    fn build_background(&self) {
        let line_no = self.current_line.get() / 8 - 1;

        // 1line = 256 pixel = 32 tile分 = ループ
        let borrowed_bus = self.bus.borrow();
        let mut graphic_buffer_mut = self.graphic_buffer.borrow_mut();
        for i in 0..32 {
            // ネームテーブルからスプライト番号を取得する
            let sprite_num = borrowed_bus.read_byte((line_no * 32 + i) + 0x2000);
            // キャラクタROMからスプライトデータを取得する
            let mut sprite = Vec::<u8>::with_capacity(16);
            for j in 0..16 {
                let sprite_line = borrowed_bus.read_byte((sprite_num as u16 * 16) + j);
                sprite.push(sprite_line);
            }

            // 属性テーブルからパレットを取り出す
            // このタイルの属性テーブルの場所
            let attribute_pos = (line_no / 4) * 8 + (i / 4);
            // 属性テーブルからアトリビュート取り出す
            let attribute = borrowed_bus.read_byte(attribute_pos + 0x23C0);

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
            let mut tile = Vec::<u8>::with_capacity(8 * 8);
            for j in 0..8 {
                let sprite_low = sprite[j];
                let sprite_high = sprite[j + 8];
                for k in (0..8).rev() {
                    let a = 0x01 << k;
                    let l = (sprite_low & a) >> k;
                    let h = ((sprite_high & a) >> k) * 2;
                    let pixel = l + h;
                    let pixel_color = self.background_palette.borrow()[(palette * 4 + pixel) as usize];
                    tile.push(pixel_color);
                }
            }

            for row in 0..8 {
                for col in 0..8 {
                    let pos = line_no * 2048 + i * 8 + row * 256 + col;
                    graphic_buffer_mut[pos as usize] = tile[(row * 8 + col) as usize];
                }
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
        // println!("[Ppu] Call write_ppu: address {:#06X}, byte {:#06X}", address, byte);
    }

    pub fn get_graphic_buffer(&self) -> Ref<Vec<u8>> {
        self.graphic_buffer.borrow()
    }

    fn write_ppu_addr(&self, byte: u8) {
        if self.is_set_ppu_high_address.get() {
            let address = self.ppu_addr.get();
            self.ppu_addr.set((address | (byte as u16)));
            self.is_set_ppu_high_address.set(false);
            // println!("[PPU] PPU address set: {:#06X}", self.ppu_addr.get());
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