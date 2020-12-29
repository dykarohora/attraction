use crate::nes::cpu::Cpu;
use crate::nes::cartridge::Cartridge;
use crate::nes::cpu_bus::CpuBus;

pub struct Emulator {
    cpu: Cpu,
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        let cpu_bus = CpuBus::new(cartridge);
        let cpu = Cpu::new(cpu_bus);
        Emulator {
            cpu
        }
    }

    pub fn start(&mut self) {
        self.cpu.reset();
        loop {
            self.cpu.run_instruction();
            println!("{:?}", self.cpu)
        }
    }
}