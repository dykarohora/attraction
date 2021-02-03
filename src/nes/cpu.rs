use std::fmt;
use std::fmt::Formatter;
use crate::nes::cpu_bus::CpuBus;
use crate::nes::opcode::{Addressing, Instruction, AddressingMode};
use crate::nes::opcode::Addressing::{Immediate, Absolute, Zeropage, AbsoluteX, AbsoluteY, ZeropageX, ZeropageY, Indirect, IndexedIndirect, IndirectIndexed, Accumulator};
use crate::nes::opcode::Instruction::{BPL, AND, JMP, SEI, STY, DEY, STA, TXS, LDY, LDX, LDA, DEC, BNE, CLD, CPX, INX, INC, BEQ, STX, DEX, JSR, RTS, CMP, BRK, PHA, TXA, LSR, ROL, TAX, TAY, TSX, TYA, EOR, PLA, RTI, CLC, CLI, CLV, SEC, SED};
use std::sync::atomic::compiler_fence;

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

impl Status {
    fn get_binary(&self) -> u8 {
        let mut binary = 0b0000_0000;
        binary = binary | if self.negative == true { 0b1000_0000 } else { 0b0000_0000 };
        binary = binary | if self.overflow == true { 0b0100_0000 } else { 0b0000_0000 };
        binary = binary | if self.break_flg == true { 0b0001_0000 } else { 0b0000_0000 };
        binary = binary | if self.decimal == true { 0b0000_1000 } else { 0b0000_0000 };
        binary = binary | if self.interrupt == true { 0b0000_0100 } else { 0b0000_0000 };
        binary = binary | if self.zero == true { 0b0000_0010 } else { 0b0000_0000 };
        binary = binary | if self.carry == true { 0b0000_0001 } else { 0b0000_0000 };
        binary
    }

    fn set_binary(&mut self, byte: u8) {
        self.negative = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
        self.negative = if byte & 0b0100_0000 == 0b0100_0000 { true } else { false };
        self.negative = if byte & 0b0001_0000 == 0b0001_0000 { true } else { false };
        self.negative = if byte & 0b0000_1000 == 0b0000_1000 { true } else { false };
        self.negative = if byte & 0b0000_0100 == 0b0000_0100 { true } else { false };
        self.negative = if byte & 0b0000_0010 == 0b0000_0010 { true } else { false };
        self.negative = if byte & 0b0000_0001 == 0b0000_0001 { true } else { false };
    }
}

impl Cpu {
    pub fn new(bus: CpuBus) -> Cpu {
        Cpu {
            a: 0x00,
            x: 0x00,
            y: 0x00,
            pc: 0x0000,
            sp: 0xff,
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

    pub fn run(&mut self, nmi: &mut bool) -> u16 {
        if *nmi {
            self.process_nmi();
            *nmi = false;
        }
        print!("{:#06X} ", self.pc);
        let opcode = self.fetch_byte();
        let instruction = self.decode_instruction(opcode);
        self.execute_instruction(instruction)
    }

    fn process_nmi(&mut self) {
        self.status.break_flg = false;
        let program_counter = self.pc;
        self.push_to_stack(((program_counter >> 8) & 0x00ff) as u8);
        self.push_to_stack((program_counter & 0x00ff) as u8);
        self.push_status_to_stack();
        self.status.interrupt = true;
        self.pc = self.read_word(0xfffa);
    }

    fn resolve_addressing(&mut self, addressing: AddressingMode) -> u16 {
        use AddressingMode::*;
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
            Indirect => {
                let word = self.fetch_word();
                self.read_word(word)
            }
            IndirectIndexed => {
                let byte = self.fetch_byte() as u16;
                let high = self.read_byte(byte) as u16;
                let low = self.read_byte(byte + 1) as u16;
                let address = ((high << 8) | low).wrapping_add(self.y as u16);
                address
            }
            IndexedIndirect => {
                let byte = self.fetch_byte();
                let base_address = byte.wrapping_add(self.x) as u16;
                self.read_word(base_address)
            }
            _ => panic!("Invalid operation")
        }
    }

    fn decode_instruction(&mut self, opcode: u8) -> Instruction {
        match opcode {
            0x00 => BRK { cycle: 7 },
            0x10 => BPL { cycle: 2 },
            0x18 => CLC { cycle: 2 },
            0x20 => JSR { cycle: 6 },
            0x21 => AND { addressing: IndexedIndirect(self.resolve_addressing(AddressingMode::IndexedIndirect)), cycle: 6 },
            0x25 => AND { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0x26 => ROL { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 5 },
            0x29 => AND { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0x2D => AND { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 4 },
            0x2E => ROL { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 6 },
            0x31 => AND { addressing: IndirectIndexed(self.resolve_addressing(AddressingMode::IndirectIndexed)), cycle: 5 },
            0x36 => ROL { addressing: ZeropageX(self.resolve_addressing(AddressingMode::ZeropageX)), cycle: 6 },
            0x38 => SEC { cycle: 2 },
            0x39 => AND { addressing: AbsoluteY(self.resolve_addressing(AddressingMode::AbsoluteY)), cycle: 4 },
            0x35 => AND { addressing: ZeropageX(self.resolve_addressing(AddressingMode::ZeropageX)), cycle: 4 },
            0x3D => AND { addressing: AbsoluteX(self.resolve_addressing(AddressingMode::AbsoluteX)), cycle: 4 },
            0x3E => ROL { addressing: AbsoluteX(self.resolve_addressing(AddressingMode::AbsoluteX)), cycle: 7 },
            0x40 => RTI { cycle: 6 },
            0x41 => EOR { addressing: IndexedIndirect(self.resolve_addressing(AddressingMode::IndexedIndirect)), cycle: 6 },
            0x45 => EOR { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0x48 => PHA { cycle: 3 },
            0x49 => EOR { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0x4A => LSR { addressing: Accumulator, cycle: 2 },
            0x4C => JMP { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 3 },
            0x4D => EOR { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 4 },
            0x51 => EOR { addressing: IndirectIndexed(self.resolve_addressing(AddressingMode::IndirectIndexed)), cycle: 5 },
            0x55 => EOR { addressing: ZeropageX(self.resolve_addressing(AddressingMode::ZeropageX)), cycle: 4 },
            0x58 => CLI { cycle: 2 },
            0x59 => EOR { addressing: AbsoluteY(self.resolve_addressing(AddressingMode::AbsoluteY)), cycle: 4 },
            0x5D => EOR { addressing: AbsoluteX(self.resolve_addressing(AddressingMode::AbsoluteX)), cycle: 4 },
            0x60 => RTS { cycle: 6 },
            0x68 => PLA { cycle: 4 },
            0x78 => SEI { cycle: 2 },
            0x84 => STY { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0x85 => STA { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0x86 => STX { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0x88 => DEY { cycle: 2 },
            0x8A => TXA { cycle: 2 },
            0x8C => STY { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 3 },
            0x8D => STA { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 4 },
            0x8E => STX { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 4 },
            0x91 => STA { addressing: IndirectIndexed(self.resolve_addressing(AddressingMode::IndirectIndexed)), cycle: 6 },
            0x96 => STX { addressing: ZeropageY(self.resolve_addressing(AddressingMode::ZeropageY)), cycle: 4 },
            0x98 => TYA { cycle: 2 },
            0x9A => TXS { cycle: 2 },
            0xA0 => LDY { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0xA1 => LDA { addressing: IndexedIndirect(self.resolve_addressing(AddressingMode::IndexedIndirect)), cycle: 6 },
            0xA2 => LDX { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0xA5 => LDA { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0xA8 => TAY { cycle: 2 },
            0xA9 => LDA { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0xAA => TAX { cycle: 2 },
            0xAD => LDA { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 4 },
            0xB1 => LDA { addressing: IndirectIndexed(self.resolve_addressing(AddressingMode::IndirectIndexed)), cycle: 5 },
            0xB5 => LDA { addressing: ZeropageX(self.resolve_addressing(AddressingMode::ZeropageX)), cycle: 4 },
            0xB8 => CLV { cycle: 2 },
            0xB9 => LDA { addressing: AbsoluteY(self.resolve_addressing(AddressingMode::AbsoluteY)), cycle: 4 },
            0xBA => TSX { cycle: 2 },
            0xBD => LDA { addressing: AbsoluteX(self.resolve_addressing(AddressingMode::AbsoluteX)), cycle: 4 },
            0xC6 => DEC { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 5 },
            0xC5 => CMP { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 3 },
            0xC9 => CMP { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0xCA => DEX { cycle: 2 },
            0xCE => DEC { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 6 },
            0xD0 => BNE { cycle: 2 },
            0xD8 => CLD { cycle: 2 },
            0xE0 => CPX { addressing: Immediate(self.resolve_addressing(AddressingMode::Immediate)), cycle: 2 },
            0xE6 => INC { addressing: Zeropage(self.resolve_addressing(AddressingMode::Zeropage)), cycle: 5 },
            0xE8 => INX { cycle: 2 },
            0xEE => INC { addressing: Absolute(self.resolve_addressing(AddressingMode::Absolute)), cycle: 6 },
            0xF0 => BEQ { cycle: 2 },
            0xF6 => INC { addressing: ZeropageX(self.resolve_addressing(AddressingMode::ZeropageX)), cycle: 6 },
            0xF8 => SED { cycle: 2 },
            0xFE => INC { addressing: AbsoluteX(self.resolve_addressing(AddressingMode::AbsoluteX)), cycle: 6 },
            _ => panic!("[CPU] Not implemented opcode: {:#04X}", opcode)
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        println!("{:?}", instruction);
        use Instruction::*;
        match instruction {
            LDA { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Absolute(operand) |
                    Zeropage(operand) |
                    AbsoluteX(operand) |
                    AbsoluteY(operand) |
                    ZeropageX(operand) |
                    ZeropageY(operand) |
                    IndirectIndexed(operand) |
                    IndexedIndirect(operand)
                    => self.lda(operand),
                    _ => panic!("Invalid Operation LDA addressing: {:?}", addressing)
                }
                cycle
            }
            LDX { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Absolute(operand) |
                    AbsoluteY(operand) |
                    Zeropage(operand) |
                    ZeropageY(operand)
                    => self.ldx(operand),
                    _ => panic!("Invalid Operation LDX addressing: {:?}", addressing)
                }
                cycle
            }
            LDY { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand) |
                    Zeropage(operand) |
                    ZeropageY(operand)
                    => self.ldy(operand),
                    _ => panic!("Invalid Operation LDY addressing: {:?}", addressing)
                }
                cycle
            }
            STA { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand) |
                    AbsoluteY(operand) |
                    IndexedIndirect(operand) |
                    IndirectIndexed(operand)
                    => self.sta(operand),
                    _ => panic!("Invalid Operation STA addressing: {:?}", addressing)
                }
                cycle
            }
            STX { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageY(operand) |
                    Absolute(operand)
                    => self.stx(operand),
                    _ => panic!("Invalid Operation STX addressing: {:?}", addressing)
                }
                cycle
            }
            STY { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand)
                    => self.stx(operand),
                    _ => panic!("Invalid Operation STY addressing: {:?}", addressing)
                }
                cycle
            }
            TAX { cycle } => {
                self.tax();
                cycle
            }
            TAY { cycle } => {
                self.tay();
                cycle
            }
            TSX { cycle } => {
                self.tsx();
                cycle
            }
            TXA { cycle } => {
                self.txa();
                cycle
            }
            TXS { cycle } => {
                self.txs();
                cycle
            }
            TYA { cycle } => {
                self.tya();
                cycle
            }
            AND { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand) |
                    AbsoluteY(operand) |
                    IndirectIndexed(operand) |
                    IndexedIndirect(operand)
                    => self.and(operand),
                    _ => panic!("Invalid Operation AND addressing: {:?}", addressing)
                }
                cycle
            }
            CMP { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand) |
                    AbsoluteY(operand) |
                    IndirectIndexed(operand) |
                    IndexedIndirect(operand)
                    => self.cmp(operand),
                    _ => panic!("Invalid Operation CMP addressing: {:?}", addressing)
                }
                cycle
            }
            CPX { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Zeropage(operand) |
                    Absolute(operand)
                    => self.cpx(operand),
                    _ => panic!("Invalid Operation CPX addressing: {:?}", addressing)
                }
                cycle
            }
            CPY { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Zeropage(operand) |
                    Absolute(operand)
                    => self.cpy(operand),
                    _ => panic!("Invalid Operation CPY addressing: {:?}", addressing)
                }
                cycle
            }
            DEC { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand)
                    => self.dec(operand),
                    _ => panic!("Invalid Operation DEC addressing: {:?}", addressing)
                }
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
            EOR { addressing, cycle, .. } => {
                match addressing {
                    Immediate(operand) |
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand) |
                    AbsoluteY(operand) |
                    IndexedIndirect(operand) |
                    IndirectIndexed(operand)
                    => self.eor(operand),
                    _ => panic!("Invalid Operation EOR addressing: {:?}", addressing)
                }
                cycle
            }
            INC { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand)
                    => self.inc(operand),
                    _ => panic!("Invalid Operation INC addressing: {:?}", addressing)
                }
                cycle
            }
            INX { cycle } => {
                self.inx();
                cycle
            }
            LSR { addressing, cycle, .. } => {
                match addressing {
                    Accumulator => self.lsr_accumulator(),
                    _ => panic!("Not implemented LSR")
                }
                cycle
            }
            ROL { addressing, cycle, .. } => {
                match addressing {
                    Zeropage(operand) |
                    ZeropageX(operand) |
                    Absolute(operand) |
                    AbsoluteX(operand)
                    => self.rol(operand),
                    _ => panic!("Invalid Operation ROL addressing: {:?}", addressing)
                }
                cycle
            }
            PHA { cycle } => {
                self.pha();
                cycle
            }
            PLA { cycle } => {
                self.pla();
                cycle
            }
            JMP { addressing, cycle, .. } => {
                match addressing {
                    Absolute(operand) |
                    Indirect(operand)
                    => self.jmp(operand),
                    _ => panic!("Invalid Operation JMP addressing: {:?}", addressing)
                }
                cycle
            }
            JSR { cycle } => {
                let operand = self.fetch_word();
                self.jsr(operand);
                cycle
            }
            RTS { cycle } => {
                self.rts();
                cycle
            }
            RTI { cycle } => {
                self.rti();
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
            CLC { cycle } => {
                self.clc();
                cycle
            }
            CLD { cycle } => {
                self.cld();
                cycle
            }
            CLI { cycle } => {
                self.cli();
                cycle
            }
            CLV { cycle } => {
                self.clv();
                cycle
            }
            SEC { cycle } => {
                self.sec();
                cycle
            }
            SED { cycle } => {
                self.sed();
                cycle
            }
            SEI { cycle } => {
                self.sei();
                cycle
            }
            BRK { cycle } => {
                self.brk();
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

    fn tax(&mut self) {
        self.x = self.a;
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x)
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y)
    }

    fn tsx(&mut self) {
        self.x = self.sp;
        self.update_negative_flag(self.x);
        self.update_zero_flag(self.x)
    }

    fn txa(&mut self) {
        self.a = self.x;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a)
    }

    fn txs(&mut self) {
        self.sp = self.x;
        self.update_negative_flag(self.sp);
        self.update_zero_flag(self.sp)
    }

    fn tya(&mut self) {
        self.a = self.y;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a)
    }

    // 算術命令
    fn and(&mut self, operand: u16) {
        self.a &= self.read_byte(operand);
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn cmp(&mut self, operand: u16) {
        self.compare(operand, self.a);
    }

    fn cpx(&mut self, operand: u16) {
        self.compare(operand, self.x);
    }

    fn cpy(&mut self, operand: u16) {
        self.compare(operand, self.y);
    }

    fn compare(&mut self, operand: u16, target_register: u8) {
        let byte = self.read_byte(operand);
        let result = target_register as i8 - byte as i8;

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

    fn eor(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        self.a = self.a ^ byte;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
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

    fn lsr_accumulator(&mut self) {
        let acc = self.a;
        self.status.carry = if (acc & 0x01) == 1 { true } else { false };
        self.a = acc >> 1;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn rol(&mut self, operand: u16) {
        let mut byte = self.read_byte(operand);
        byte = byte << 1;

        if self.status.carry == true {
            byte += 1;
        }

        self.write_byte(operand, byte);
        if byte & 0b1000_0000 == 0b1000_0000 {
            self.status.carry = true
        } else {
            self.status.carry = false
        }
        self.update_zero_flag(byte);
        self.update_negative_flag(byte);
    }

    // スタック命令
    fn pha(&mut self) {
        self.push_to_stack(self.a);
    }

    fn pla(&mut self) {
        self.a = self.pop_from_stack();
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    // ジャンプ命令
    fn jmp(&mut self, operand: u16) {
        self.pc = operand
    }

    fn jsr(&mut self, operand: u16) {
        // 6502のJSR命令では次の命令の1つ前のアドレス(JSRの最後のバイト)をスタックに入れる
        let program_counter = self.pc - 1;
        self.push_to_stack(((program_counter >> 8) & 0x00ff) as u8);
        self.push_to_stack((program_counter & 0x00ff) as u8);
        self.pc = operand
    }

    fn rts(&mut self) {
        let low = self.pop_from_stack() as u16;
        let high = self.pop_from_stack() as u16;
        // JSR命令ではJSR命令の次の命令の1つ前のアドレスがスタックに入っているので、
        // RTS命令で復帰する時はスタックからポップしたアドレスをインクリメントしてプログラムカウンタにセットする
        let address = ((high << 8) | low) + 1;
        self.pc = address;
    }

    fn rti(&mut self) {
        self.pop_status_from_stack();
        let low = self.pop_from_stack() as u16;
        let high = self.pop_from_stack() as u16;
        self.pc = (high << 8) | low;
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
    fn clc(&mut self) {
        self.status.carry = false;
    }

    fn cld(&mut self) {
        self.status.decimal = false;
    }

    fn cli(&mut self) {
        self.status.interrupt = false;
    }

    fn clv(&mut self) {
        self.status.overflow = false;
    }

    fn sec(&mut self) {
        self.status.carry = true;
    }

    fn sed(&mut self) {
        self.status.decimal = true;
    }

    fn sei(&mut self) {
        self.status.interrupt = true;
    }

    // その他
    fn brk(&mut self) {
        self.status.break_flg = true;
        self.pc += 1;
        self.push_to_stack(((self.pc >> 8) | 0x00ff) as u8);
        self.push_to_stack((self.pc | 0x00ff) as u8);
        self.push_status_to_stack();
        if self.status.interrupt == false {
            self.pc = self.read_word(0xfffe);
        }
        self.status.interrupt = true;
        self.pc -= 1;
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


    fn push_status_to_stack(&mut self) {
        let byte = self.status.get_binary();
        self.push_to_stack(byte);
    }

    fn pop_status_from_stack(&mut self) {
        let byte = self.pop_from_stack();
        self.status.set_binary(byte);
    }

    fn push_to_stack(&mut self, byte: u8) {
        self.bus.write_byte(0x0100 | (self.sp as u16), byte);
        self.sp -= 1;
    }

    fn pop_from_stack(&mut self) -> u8 {
        self.sp += 1;
        self.bus.read_byte(0x0100 | (self.sp as u16))
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
