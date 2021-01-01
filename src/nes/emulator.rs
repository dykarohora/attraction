use crate::nes::cpu::Cpu;
use crate::nes::cartridge::Cartridge;
use crate::nes::cpu_bus::CpuBus;
use std::rc::Rc;
use crate::nes::ppu::Ppu;
use std::io::{Read, BufRead};
use crate::nes::ppu_bus::PpuBus;
use std::cell::Ref;


pub struct Emulator {
    cpu: Cpu,
    ppu: Rc<Ppu>,
    cycle_count: u16
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
            cycle_count: 0
        }
    }

    pub fn start(&mut self) {
        self.cpu.reset();
    }

    pub fn frame(&mut self) {
        loop {
            let cycle = self.cpu.run_instruction();
            self.cycle_count += cycle;
            self.ppu.run(cycle * 3);

            if self.cycle_count >= 29781 {
                self.cycle_count = self.cycle_count % 29781;
                break
            }
        }
        // println!("{:?}", self.cpu);
    }

    pub fn get_graphic_buffer(&self) -> Ref<Vec<u32>> {
        self.ppu.get_graphic_buffer()
    }
}