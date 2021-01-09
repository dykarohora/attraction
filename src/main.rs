use minifb::{Window, WindowOptions, Key, KeyRepeat};

use std::fs::File;
use std::env;
use std::io::Read;

use attraction::nes::cartridge::Cartridge;
use attraction::nes::emulator::Emulator;
use attraction::nes::color::COLOR;
use std::time::{Instant, Duration};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = match args.len() {
        0 | 1 => "rom/donkey-kong.nes",
        _ => args.get(1).unwrap(),
    };

    let mut file = File::open(file_name).unwrap();
    let mut data = Vec::<u8>::new();
    file.read_to_end(&mut data).expect("Failed read file");
    data.shrink_to_fit();
    let cartridge = Cartridge::load_rom(&data);

    let width = 256;
    let height = 240;
    let mut window = Window::new(
        "ATTRACTION",
        width,
        height,
        WindowOptions::default(),
    ).unwrap_or_else(|e| {
        panic!("Window creation failed: {:?}", e);
    });

    let mut emulator = Emulator::new(cartridge);
    emulator.start();

    let mut last_display_time = Instant::now();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        last_display_time = Instant::now();
        let mut key_buffer: u8 = 0;
        if window.is_key_down(Key::D) {
            key_buffer |= 0b0000_0001
        }
        if window.is_key_down(Key::S) {
            key_buffer |= 0b0000_0010
        }
        if window.is_key_down(Key::X) {
            key_buffer |= 0b0000_0100
        }
        if window.is_key_down(Key::C) {
            key_buffer |= 0b0000_1000
        }
        if window.is_key_down(Key::K) {
            key_buffer |= 0b0001_0000
        }
        if window.is_key_down(Key::J) {
            key_buffer |= 0b0010_0000
        }
        if window.is_key_down(Key::H) {
            key_buffer |= 0b0100_0000
        }
        if window.is_key_down(Key::L) {
            key_buffer |= 0b1000_0000
        }

        emulator.update_key_buffer(key_buffer);
        emulator.frame();
        window.update_with_buffer(&emulator.get_graphic_buffer(), width, height);
        let duration = Instant::now() - last_display_time;
        println!("render duration: {:?}", duration);
    }

    println!("complete");
    let mut number = String::new();
    std::io::stdin().read_line(&mut number).ok();
}
