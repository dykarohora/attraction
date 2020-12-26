use super::rom::Rom;

pub struct Cartridge {
    program_rom: Rom,
    character_rom: Rom,
    mapper: u8,
    is_horizontal_mirror: bool,
}

impl Cartridge {
    pub fn load_rom(data: &Vec<u8>) -> Cartridge {
        println!("Cartridge Size: {} byte capacity: {} byte", data.len(), data.capacity());

        let magic_code = String::from_utf8(data[..3].to_vec());
        match magic_code {
            Ok(str) => {
                if str.as_str() != "NES" {
                    panic!("invalid magic_code")
                }
            }
            Err(e) => panic!("ng") // 独自のエラー型に変換する
        }

        // プログラムROMのサイズを取得する
        let program_rom_size = 0x4000 * data[4] as usize;
        // キャラクタROMのサイズを取得する
        let character_rom_size = 0x2000 * data[5] as usize;
        // flag6を取得する
        let flag_six = data[6];
        // flag7を取得する
        let flag_seven = data[7];

        Cartridge {
            program_rom: Rom::new(data[16..(program_rom_size + 16)].to_vec()),
            character_rom: Rom::new(data[(16 + program_rom_size)..(16 + program_rom_size + character_rom_size)].to_vec()),
            mapper: (flag_seven & 0xF0) | (flag_six >> 4),
            is_horizontal_mirror: if flag_six & 0x01 == 0x01 { true } else { false },
        }
    }
}
