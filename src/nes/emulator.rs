use crate::nes::cpu::Cpu;
use crate::nes::cartridge::Cartridge;
use crate::nes::cpu_bus::CpuBus;
use std::rc::Rc;

pub struct Emulator {
    cpu: Cpu,
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        let cartridge_rc = Rc::new(cartridge);
        let cpu_bus = CpuBus::new(cartridge_rc.clone());
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