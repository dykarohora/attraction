use std::fmt;
use crate::nes::cpu_bus::CpuBus;
use std::fmt::Formatter;

#[derive(Default)]
pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: Status,
    bus: CpuBus,
}

#[derive(Default, Debug)]
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
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.status.break_flg = true;
        self.status.interrupt = true;
        self.sp = 0xFD;
        self.pc = self.read_word(0xFFFC);
    }

    pub fn run_instruction(&mut self) -> u16 {
        let opcode = self.fetch_byte();
        print!("opcode: {:#04X} ", opcode);

        let cycle = match opcode {
            0x10 => self.bpl(),
            0x29 => self.and_immediate(),
            0x4C => self.jmp_absolute(),
            0x78 => self.sei(),
            0x88 => self.dey(),
            0x8D => self.sta_absolute(),
            0x9A => self.txs(),
            0xA0 => self.ldy_immediate(),
            0xA2 => self.ldx_immediate(),
            0xA9 => self.lda_immediate(),
            0xAD => self.lda_absolute(),
            0xBD => self.lda_absolute_x(),
            0xCE => self.dec_absolute(),
            0xD0 => self.bne(),
            0xD8 => self.cld(),
            0xE0 => self.cpx_immediate(),
            0xE8 => self.inx(),
            0xEE => self.inc_absolute(),
            0xF0 => self.beq(),
            _ => panic!("[Cpu] Not implemented opcode {:#04X}", opcode)
        };

        println!();
        cycle
    }

    fn fetch_byte(&mut self) -> u8 {
        let byte = self.read_byte(self.pc);
        self.pc += 1;
        byte
    }

    fn fetch_word(&mut self) -> u16 {
        let word = self.read_word(self.pc);
        self.pc += 2;
        word
    }

    fn sei(&mut self) -> u16 {
        // println!("SEI immediate");
        self.status.interrupt = true;
        2
    }

    fn ldx_immediate(&mut self) -> u16 {
        let operand = self.fetch_byte();
        self.x = operand;

        self.status.negative = if (self.x & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.x == 0 { true } else { false };

        print!("LDX immediate {:#06X}", self.x);
        2
    }

    fn ldy_immediate(&mut self) -> u16 {
        let operand = self.fetch_byte();
        self.y = operand;

        self.status.negative = if (self.y & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.y == 0 { true } else { false };

        // println!("LDY immediate {:#06X}", self.y);

        2
    }

    fn lda_immediate(&mut self) -> u16 {
        let operand = self.fetch_byte();
        self.a = operand;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        print!("LDA immediate {:#06X}", self.a);

        2
    }

    fn lda_absolute(&mut self) -> u16 {
        let address = self.fetch_word();
        let byte = self.read_byte(address);
        self.a = byte;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        4
    }

    fn lda_absolute_x(&mut self) -> u16 {
        let address = self.fetch_word();
        let byte = self.read_byte(address + self.x as u16);
        self.a = byte;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        print!("LDA absolute {:#06X}", self.a);
        4
    }

    fn sta_absolute(&mut self) -> u16 {
        let address = self.fetch_word();
        self.write_byte(address, self.a);

        print!("STA absolute address:{:#06X} register_a:{:#06X}", address, self.a);
        4
    }

    fn and_immediate(&mut self) -> u16 {
        let operand = self.fetch_byte();
        self.a &= operand;

        self.status.negative = if (self.a & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.a == 0 { true } else { false };

        print!("AND immediate A:{:#06X} OP:{:#06X}", self.a, operand);

        2
    }

    fn inx(&mut self) -> u16 {
        print!("INX");
        self.x = self.x.wrapping_add(1);

        self.status.negative = if (self.x & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.x == 0 { true } else { false };

        2
    }

    fn inc_absolute(&mut self) -> u16 {
        let address = self.fetch_word();
        let byte = self.read_byte(address);
        let result = byte.wrapping_add(1);
        self.write_byte(address, result);

        self.status.negative = if (result & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if result == 0 { true } else { false };

        6
    }

    fn dec_absolute(&mut self) -> u16 {
        let address = self.fetch_word();
        let byte = self.read_byte(address);
        let result = byte.wrapping_sub(1);
        self.write_byte(address, result);

        self.status.negative = if (result & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if result == 0 { true } else { false };

        6
    }

    fn dey(&mut self) -> u16 {
        print!("DEY");
        self.y = self.y.wrapping_sub(1);

        self.status.negative = if (self.y & 0b1000_0000) >> 7 == 1 { true } else { false };
        self.status.zero = if self.y == 0 { true } else { false };

        2
    }

    fn bpl(&mut self) -> u16 {
        let mut offset = self.fetch_byte();

        if self.status.negative == false {
            let is_negative = (offset & 0b1000_0000) == 0b1000_0000;

            match is_negative {
                true => {
                    offset = !offset + 1;
                    self.pc -= offset as u16
                }
                false => { self.pc += offset as u16 }
            }
        }

        2
    }

    fn beq(&mut self) -> u16 {
        let mut offset = self.fetch_byte();
        print!("BEQ offset:{:#06X}", offset);

        if self.status.zero == true {
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

        2
    }

    fn bne(&mut self) -> u16 {
        let mut offset = self.fetch_byte();
        print!("BNE offset:{:#06X}", offset);

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

        2
    }

    fn cpx_immediate(&mut self) -> u16 {
        let operand = self.fetch_byte();

        print!("CPX compare X:{:#06X} to operand:{:#06X}", self.x, operand);

        let result = self.x as i8 - operand as i8;

        self.status.zero = if result == 0 { true } else { false };

        if result >= 0 {
            self.status.negative = false;
            self.status.carry = true;
        } else {
            self.status.negative = true;
            self.status.carry = false;
        };

        2
    }

    fn jmp_absolute(&mut self) -> u16 {
        // println!("JMP absolute");
        let address = self.fetch_word();
        self.pc = address;
        3
    }

    fn txs(&mut self) -> u16 {
        // println!("TXS");
        self.sp = self.x;

        2
    }

    fn cld(&mut self) -> u16 {
        2
    }

    fn read_byte(&self, address: u16) -> u8 {
        self.bus.read_byte(address)
    }

    fn read_word(&self, address: u16) -> u16 {
        let low = self.bus.read_byte(address) as u16;
        let high = self.bus.read_byte(address + 1) as u16;
        (high << 8) | low
    }

    fn write_byte(&mut self, address: u16, byte: u8) {
        self.bus.write_byte(address, byte)
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
