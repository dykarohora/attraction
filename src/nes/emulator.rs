use crate::nes::cpu::Cpu;
use crate::nes::cartridge::Cartridge;
use crate::nes::cpu_bus::CpuBus;
use std::rc::Rc;
use crate::nes::ppu::Ppu;
use std::io::{Read, BufRead};
use crate::nes::ppu_bus::PpuBus;

pub struct Emulator {
    cpu: Cpu,
    ppu: Rc<Ppu>,
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        let cartridge_rc = Rc::new(cartridge);
        let ppu_bus = PpuBus::new(cartridge_rc.clone());
        let ppu = Ppu::new(ppu_bus);
        let ppu_rc = Rc::new(ppu);
        let cpu_bus = CpuBus::new(ppu_rc.clone(), cartridge_rc.clone());
        let cpu = Cpu::new(cpu_bus);
        Emulator {
            cpu,
            ppu: ppu_rc,
        }
    }

    pub fn start(&mut self) {
        self.cpu.reset();
        loop {
            self.cpu.run_instruction();
            self.ppu.run();
            println!("{:?}", self.cpu);
            // let mut number = String::new();
            // std::io::stdin().read_line(&mut number).ok();
        }
    }
}