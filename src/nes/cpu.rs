use std::fmt;
use crate::nes::cpu_bus::CpuBus;
use std::fmt::Formatter;
use crate::nes::cartridge::Cartridge;
use crate::nes::ppu::Ppu;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: Status,
    total_cycle: u64,
    bus: CpuBus,
}

struct Status {
    negative: bool,
    overflow: bool,
    break_flg: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,
}

impl Cpu {
    pub fn new(bus: CpuBus) -> Cpu {
        Cpu {
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x0000,
            sp: 0x00,
            status: Status {
                negative: false,
                overflow: false,
                break_flg: false,
                decimal: false,
                interrupt: false,
                zero: false,
                carry: false,
            },
            total_cycle: 0,
            bus,
        }
    }

    pub fn reset(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        self.total_cycle = 0;
        self.status.break_flg = true;
        self.status.interrupt = true;
        self.sp = 0xFD;
        self.pc = self.read_word(cartridge, ppu, 0xFFFC);
    }

    pub fn run_instruction(&mut self, cartridge: &mut Cartridge, ppu: &mut Ppu) {
        let opcode = self.fetch_byte(cartridge, ppu);
        println!("opcode: {:#04X}", opcode);
        match opcode {
            0x4C => self.jmp_absolute(cartridge, ppu),
            0x78 => self.sei(),
            0x88 => self.dey(),
            0x8D => self.sta_absolute(cartridge, ppu),
            0x9A => self.txs(),
            0xA0 => self.ldy_immediate(cartridge, ppu),
            0xA2 => self.ldx_immediate(cartridge, ppu),
            0xA9 => self.lda_immediate(cartridge, ppu),
            0xBD => self.lda_absolute_x(cartridge, ppu),
            0xD0 => self.bne(cartridge, ppu),
            0xE8 => self.inx(),
            _ => panic!("[Cpu] Not implemented opcode {:#04X}", opcode)
        }
    }

    fn fetch_byte(&mut self, cartridge: &Cartridge, ppu: &Ppu) -> u8 {
        let byte = self.read_byte(cartridge, ppu, self.pc);
        self.pc += 1;
        byte
    }

    fn fetch_word(&mut self, cartridge: &Cartridge, ppu: &Ppu) -> u16 {
        let word = self.read_word(cartridge, ppu, self.pc);
        self.pc += 2;
        word
    }

    fn sei(&mut self) {
        self.status.interrupt = true;
        self.total_cycle += 2
    }

    fn ldx_immediate(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let operand = self.fetch_byte(cartridge, ppu);
        self.x = operand;

        self.status.negative = if (self.x & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.x == 0 { true } else { false };

        self.total_cycle += 2
    }

    fn ldy_immediate(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let operand = self.fetch_byte(cartridge, ppu);
        self.y = operand;

        self.status.negative = if (self.y & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.y == 0 { true } else { false };

        self.total_cycle += 2
    }

    fn lda_immediate(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let operand = self.fetch_byte(cartridge, ppu);
        self.a = operand;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        self.total_cycle += 2
    }

    fn lda_absolute_x(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let address = self.fetch_word(cartridge, ppu);
        let byte = self.read_byte(cartridge, ppu, address + self.x as u16);
        self.a = byte;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        self.total_cycle += 4
    }

    fn sta_absolute(&mut self, cartridge: &mut Cartridge, ppu: &mut Ppu) {
        let address = self.fetch_word(cartridge, ppu);
        self.write_byte(cartridge, ppu, address, self.a);

        self.total_cycle += 4
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);

        self.status.negative = if (self.x & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.x == 0 { true } else { false };

        self.total_cycle += 2
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);

        self.status.negative = if (self.y & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.y == 0 { true } else { false };

        self.total_cycle += 2;
    }

    fn bne(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let mut offset = self.fetch_byte(cartridge, ppu);

        if self.status.zero == false {
            let is_negative = (offset & 0b1000_0000) == 0b1000_0000;

            match is_negative {
                true => {
                    offset = !offset + 1;
                    // self.pc.wrapping_sub(offset as u16)
                    self.pc -= offset as u16
                }
                false => { self.pc += offset as u16 }
            };
        }

        self.total_cycle += 2
    }

    fn jmp_absolute(&mut self, cartridge: &Cartridge, ppu: &Ppu) {
        let address = self.fetch_word(cartridge, ppu);
        self.pc = address;
        self.total_cycle += 3
    }

    fn txs(&mut self) {
        self.sp = self.x;

        self.total_cycle += 2
    }

    fn read_byte(&self, cartridge: &Cartridge, ppu: &Ppu, address: u16) -> u8 {
        self.bus.read_byte(cartridge, ppu, address)
    }

    fn read_word(&self, cartridge: &Cartridge, ppu: &Ppu, address: u16) -> u16 {
        let low = self.bus.read_byte(cartridge, ppu, address) as u16;
        let high = self.bus.read_byte(cartridge, ppu, address + 1) as u16;
        (high << 8) | low
    }

    fn write_byte(&mut self, cartridge: &mut Cartridge, ppu: &mut Ppu, address: u16, byte: u8) {
        self.bus.write_byte(cartridge, ppu, address, byte)
    }
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "A:{:#04X}, X:{:#04X}, Y:{:#04X}\n", self.a, self.x, self.y);
        write!(f, "PC:{:#06X}, SP:{:#04X}\n", self.pc, self.sp);
        write!(f, "[Status]\n");
        write!(f, "N:{}\n", self.status.negative);
        write!(f, "V:{}\n", self.status.overflow);
        write!(f, "B:{}\n", self.status.break_flg);
        write!(f, "D:{}\n", self.status.decimal);
        write!(f, "I:{}\n", self.status.interrupt);
        write!(f, "Z:{}\n", self.status.zero);
        write!(f, "C:{}\n", self.status.carry)
    }
}
