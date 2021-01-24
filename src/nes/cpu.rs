use std::fmt;
use std::fmt::Formatter;
use crate::nes::cpu_bus::CpuBus;
use crate::nes::opcode::{Addressing, Instruction};
use crate::nes::opcode::Addressing::{Immediate, Absolute, Zeropage, IndirectIndexed, AbsoluteX};
use crate::nes::opcode::Instruction::{BPL, AND, JMP, SEI, STY, DEY, STA, TXS, LDY, LDX, LDA, DEC, BNE, CLD, CPX, INX, INC, BEQ, STX, DEX};

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

    pub fn run(&mut self) -> u16 {
        let opcode = self.fetch_byte();
        println!("opcode: {:#04X}", opcode);
        let instruction = self.decode_instruction(opcode);
        self.execute_instruction(instruction)
    }

    fn resolve_addressing(&mut self, addressing: Addressing) -> u16 {
        use Addressing::*;
        match addressing {
            Immediate => {
                let address = self.pc;
                self.pc += 1;
                address
            }
            Absolute => self.fetch_word(),
            AbsoluteX => self.fetch_word() + (self.x as u16),
            AbsoluteY => self.fetch_word() + (self.y as u16),
            Zeropage => self.fetch_byte() as u16,
            ZeropageX => {
                let byte = self.fetch_byte();
                byte.wrapping_add(self.x) as u16
            }
            ZeropageY => {
                let byte = self.fetch_byte();
                byte.wrapping_add(self.y) as u16
            }
            IndirectIndexed => {
                let temp_address = self.fetch_byte() as u16;
                let high = self.read_byte(temp_address) as u16;
                let low = self.read_byte(temp_address + 1) as u16;
                let address = ((high << 8) | low).wrapping_add(self.y as u16);
                address
            }
        }
    }

    fn decode_instruction(&mut self, opcode: u8) -> Instruction {
        match opcode {
            0x10 => BPL { cycle: 2 },
            0x29 => AND { operand: self.resolve_addressing(Immediate), addressing: Immediate, cycle: 2 },
            0x4C => JMP { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 3 },
            0x78 => SEI { cycle: 2 },
            0x84 => STY { operand: self.resolve_addressing(Zeropage), addressing: Zeropage, cycle: 3 },
            0x85 => STA { operand: self.resolve_addressing(Zeropage), addressing: Zeropage, cycle: 3 },
            0x88 => DEY { cycle: 2 },
            0x8C => STY { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 3 },
            0x8D => STA { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 4 },
            0x8E => STX { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 4 },
            0x91 => STA { operand: self.resolve_addressing(IndirectIndexed), addressing: IndirectIndexed, cycle: 6 },
            0x9A => TXS { cycle: 2 },
            0xA0 => LDY { operand: self.resolve_addressing(Immediate), addressing: Immediate, cycle: 2 },
            0xA2 => LDX { operand: self.resolve_addressing(Immediate), addressing: Immediate, cycle: 2 },
            0xA9 => LDA { operand: self.resolve_addressing(Immediate), addressing: Immediate, cycle: 2 },
            0xAD => LDA { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 3 },
            0xBD => LDA { operand: self.resolve_addressing(AbsoluteX), addressing: AbsoluteX, cycle: 4 },
            0xC6 => DEC { operand: self.resolve_addressing(Zeropage), addressing: Zeropage, cycle: 5 },
            0xCA => DEX { cycle: 2 },
            0xCE => DEC { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 6 },
            0xD0 => BNE { cycle: 2 },
            0xD8 => CLD { cycle: 2 },
            0xE0 => CPX { operand: self.resolve_addressing(Immediate), addressing: Immediate, cycle: 2 },
            0xE8 => INX { cycle: 2 },
            0xEE => INC { operand: self.resolve_addressing(Absolute), addressing: Absolute, cycle: 6 },
            0xF0 => BEQ { cycle: 2 },
            _ => panic!("[CPU] Not implemented opcode: {:#04X}", opcode)
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        println!("{:?}", instruction);
        use Instruction::*;
        match instruction {
            LDA { operand, cycle, .. } => {
                self.lda(operand);
                cycle
            }
            LDX { operand, cycle, .. } => {
                self.ldx(operand);
                cycle
            }
            LDY { operand, cycle, .. } => {
                self.ldy(operand);
                cycle
            }
            STA { operand, cycle, .. } => {
                self.sta(operand);
                cycle
            }
            STX { operand, cycle, .. } => {
                self.stx(operand);
                cycle
            }
            STY { operand, cycle, .. } => {
                self.sty(operand);
                cycle
            }
            TXS { cycle } => {
                self.txs();
                cycle
            }

            AND { operand, cycle, .. } => {
                self.and(operand);
                cycle
            }
            CPX { operand, cycle, .. } => {
                self.cpx(operand);
                cycle
            }
            DEC { operand, cycle, .. } => {
                self.dec(operand);
                cycle
            }
            DEX { cycle } => {
                self.dex();
                cycle
            }
            DEY { cycle } => {
                self.dey();
                cycle
            }
            INC { operand, cycle, .. } => {
                self.inc(operand);
                cycle
            }
            INX { cycle } => {
                self.inx();
                cycle
            }

            JMP { operand, cycle, .. } => {
                self.jmp(operand);
                cycle
            }

            BEQ { cycle } => {
                self.beq();
                cycle
            }
            BNE { cycle } => {
                self.bne();
                cycle
            }
            BPL { cycle } => {
                self.bpl();
                cycle
            }

            CLD { cycle } => {
                self.cld();
                cycle
            }
            SEI { cycle } => {
                self.sei();
                cycle
            }
            _ => panic!("not implemented")
        }
    }

    // 転送命令
    fn lda(&mut self, operand: u16) {
        self.a = self.read_byte(operand);
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn ldx(&mut self, operand: u16) {
        self.x = self.read_byte(operand);
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn ldy(&mut self, operand: u16) {
        self.y = self.read_byte(operand);
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn sta(&mut self, operand: u16) {
        self.write_byte(operand, self.a);
    }

    fn stx(&mut self, operand: u16) {
        self.write_byte(operand, self.x);
    }

    fn sty(&mut self, operand: u16) {
        self.write_byte(operand, self.y);
    }

    fn txs(&mut self) {
        self.sp = self.x;
    }

    // 算術命令
    fn and(&mut self, operand: u16) {
        self.a &= self.read_byte(operand);
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn cpx(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = self.x as i8 - byte as i8;

        self.status.zero = if result == 0 { true } else { false };

        if result >= 0 {
            self.status.negative = false;
            self.status.carry = true;
        } else {
            self.status.negative = true;
            self.status.carry = false;
        };
    }

    fn dec(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = byte.wrapping_sub(1);
        self.write_byte(operand, result);
        self.update_negative_flag(result);
        self.update_zero_flag(result);
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn inc(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = byte.wrapping_add(1);
        self.write_byte(operand, result);
        self.update_negative_flag(result);
        self.update_zero_flag(result);
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x);
    }

    // ジャンプ命令
    fn jmp(&mut self, operand: u16) {
        self.pc = operand
    }

    // 分岐命令
    fn beq(&mut self) {
        let mut offset = self.fetch_byte();

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
    }

    fn bne(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.zero == false {
            let is_negative = (offset & 0b1000_0000) == 0b1000_0000;

            match is_negative {
                true => {
                    offset = !offset + 1;
                    self.pc -= offset as u16
                }
                false => { self.pc += offset as u16 }
            };
        }
    }

    fn bpl(&mut self) {
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
    }

    // フラグ変更命令
    fn cld(&mut self) {}

    fn sei(&mut self) -> u16 {
        self.status.interrupt = true;
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

    fn update_negative_flag(&mut self, calculation_result: u8) {
        self.status.negative = if (calculation_result & 0b1000_0000) >> 7 == 1 { true } else { false };
    }

    fn update_zero_flag(&mut self, calculation_result: u8) {
        self.status.zero = if calculation_result == 0 { true } else { false };
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
