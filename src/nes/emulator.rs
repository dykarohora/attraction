use crate::nes::cpu::Cpu;
use crate::nes::cpu_bus::CpuBus;
use crate::nes::ppu::Ppu;
use crate::nes::cartridge::Cartridge;

pub struct Emulator {
    cpu: Cpu,
    ppu: Ppu,
    cartridge: Cartridge
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        let cpu = Cpu::new(CpuBus::new());
        let ppu = Ppu::new();
        Emulator {
            cpu,
            ppu,
            cartridge
        }
    }

    pub fn start(&mut self) {
        self.cpu.reset(&self.cartridge, &self.ppu);
        loop {
            self.cpu.run_instruction(&mut self.cartridge, &mut self.ppu);
            println!("{:?}", self.cpu);
            self.ppu.run(&self.cartridge);
        }
    }
}