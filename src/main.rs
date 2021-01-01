use minifb::{Window, WindowOptions};

use std::fs::File;
use std::env;
use std::io::Read;

use attraction::nes::cartridge::Cartridge;
use attraction::nes::emulator::Emulator;
use attraction::nes::color::COLOR;
use std::time::Instant;

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
    let cartridge = Cartridge::load_rom(&data);

    let width = 256;
    let height = 240;
    let mut window_buffer: Vec<u32> = vec![0; width * height];
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
    let mut count: usize = 0;
    while count < 10 {
        last_display_time = Instant::now();
        emulator.frame();
        let duration_emu = Instant::now() - last_display_time;
        println!("frame duration {:?}", duration_emu);

        last_display_time = Instant::now();
        let graphic_buffer = emulator.get_graphic_buffer();
        for i in 0..(width*height) {
            let pixel = graphic_buffer[i];
            window_buffer[i] = COLOR[pixel as usize];
        }
        window.update_with_buffer(&window_buffer, width, height);
        let duration_renderer = Instant::now() - last_display_time;
        println!("render duration {:?}", duration_renderer);
        count += 1;
    }
    println!("complete");
    let mut number = String::new();
    std::io::stdin().read_line(&mut number).ok();
}
