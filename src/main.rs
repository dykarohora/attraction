use std::fs::File;
use std::env;
use std::io::Read;

use attraction::nes::cartridge::Cartridge;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = match args.len() {
        0 | 1 => "rom/helloworld.nes",
        _ => args.get(1).unwrap(),
    };

    let mut file = File::open(file_name).unwrap();
    let mut data = Vec::<u8>::new();
    file.read_to_end(&mut data).expect("Failed read file");
    data.shrink_to_fit();
    Cartridge::load_rom(&data);
    println!("complete");
}
