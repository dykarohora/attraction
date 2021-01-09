use crate::nes::cpu::Cpu;
use crate::nes::cartridge::Cartridge;
use crate::nes::cpu_bus::CpuBus;
use std::rc::Rc;
use crate::nes::ppu::Ppu;
use std::io::{Read, BufRead};
use crate::nes::ppu_bus::PpuBus;
use std::cell::{Ref, RefCell};
use crate::nes::keypad::KeyPad;
use minifb::Key;


pub struct Emulator {
    cpu: Cpu,
    ppu: Rc<RefCell<Ppu>>,
    cycle_count: u16,
    keypad: Rc<RefCell<KeyPad>>
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        let keypad = Rc::new(RefCell::new(KeyPad::default()));
        let cartridge_rc = Rc::new(cartridge);
        let ppu_bus = PpuBus::new(cartridge_rc.clone());
        let ppu = Ppu::new(ppu_bus);
        let ppu_rc = Rc::new(RefCell::new(ppu));
        let cpu_bus = CpuBus::new(ppu_rc.clone(), cartridge_rc.clone(), keypad.clone());
        let cpu = Cpu::new(cpu_bus);

        Emulator {
            cpu,
            ppu: ppu_rc,
            cycle_count: 0,
            keypad
        }
    }

    pub fn start(&mut self) {
        self.cpu.reset();
    }

    pub fn frame(&mut self) {
        loop {
            let cycle = self.cpu.run_instruction();
            self.cycle_count += cycle;
            self.ppu.borrow_mut().run(cycle * 3);

            if self.cycle_count >= 29781 {
                self.cycle_count = self.cycle_count % 29781;
                break;
            }
        }
        // println!("{:?}", self.cpu);
    }

    pub fn get_graphic_buffer(&self) -> Vec<u32> {
        self.ppu.borrow().get_graphic_buffer().clone()
    }

    pub fn update_key_buffer(&self, buffer: u8) {
        self.keypad.borrow_mut().update_key_buffer_for_player1(buffer)
    }
}