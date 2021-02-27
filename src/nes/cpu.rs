use std::fmt;
use std::fmt::Formatter;
use crate::nes::cpu_bus::CpuBus;
use crate::nes::opcode::{Instruction, AddressingMode};
use crate::nes::opcode::Instruction::{BPL, AND, JMP, SEI, STY, DEY, STA, TXS, LDY, LDX, LDA, DEC, BNE, CLD, CPX, INX, INC, BEQ, STX, DEX, JSR, RTS, CMP, BRK, PHA, TXA, LSR, ROL, TAX, TAY, TSX, TYA, EOR, PLA, RTI, CLC, CLI, CLV, SEC, SED, ADC, INY, BCC, BCS, BMI, BVC, BVS, ASL, ROR, NOP, CPY, BIT, ORA, PHP, PLP, SBC, SLO, RLA, DOP, TOP};
use std::sync::atomic::compiler_fence;
use crate::nes::opcode::AddressingMode::{Immediate, Zeropage, ZeropageX, Absolute, AbsoluteY, AbsoluteX, IndexedIndirect, IndirectIndexed, ZeropageY, Accumulator, Indirect};

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

#[derive(Default)]
struct Status {
    negative: bool,
    overflow: bool,
    unused: bool,
    break_flg: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,
}

impl fmt::Debug for Status {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let n = if self.negative == true { "N" } else { " " };
        let v = if self.overflow == true { "V" } else { " " };
        let u = if self.unused == true { "U" } else { " " };
        let b = if self.break_flg == true { "B" } else { " " };
        let d = if self.decimal == true { "D" } else { " " };
        let i = if self.interrupt == true { "I" } else { " " };
        let z = if self.zero == true { "Z" } else { " " };
        let c = if self.carry == true { "C" } else { " " };
        write!(f, "{}{}{}{}{}{}{}{}", n, v, u, b, d, i, z, c)
    }
}

impl Status {
    fn get_binary(&self) -> u8 {
        let mut binary = 0b0000_0000;
        binary = binary | if self.negative == true { 0b1000_0000 } else { 0b0000_0000 };
        binary = binary | if self.overflow == true { 0b0100_0000 } else { 0b0000_0000 };
        binary = binary | if self.unused == true { 0b0010_0000 } else { 0b0000_0000 };
        binary = binary | if self.break_flg == true { 0b0001_0000 } else { 0b0000_0000 };
        binary = binary | if self.decimal == true { 0b0000_1000 } else { 0b0000_0000 };
        binary = binary | if self.interrupt == true { 0b0000_0100 } else { 0b0000_0000 };
        binary = binary | if self.zero == true { 0b0000_0010 } else { 0b0000_0000 };
        binary = binary | if self.carry == true { 0b0000_0001 } else { 0b0000_0000 };
        binary
    }

    fn set_binary(&mut self, byte: u8) {
        self.negative = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
        self.overflow = if byte & 0b0100_0000 == 0b0100_0000 { true } else { false };
        self.unused = if byte & 0b0010_0000 == 0b0010_0000 { true } else { false };
        self.break_flg = if byte & 0b0001_0000 == 0b0001_0000 { true } else { false };
        self.decimal = if byte & 0b0000_1000 == 0b0000_1000 { true } else { false };
        self.interrupt = if byte & 0b0000_0100 == 0b0000_0100 { true } else { false };
        self.zero = if byte & 0b0000_0010 == 0b0000_0010 { true } else { false };
        self.carry = if byte & 0b0000_0001 == 0b0000_0001 { true } else { false };
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
                unused: false,
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
        self.status.interrupt = true;
        self.status.unused = true;
        self.sp = 0xFD;
        self.pc = self.read_word(0xFFFC);
    }

    pub fn run(&mut self, nmi: &mut bool) -> u16 {
        if *nmi {
            println!("Enter NMI");
            self.process_nmi();
            *nmi = false;
        }
        print!("{:#06X} A:{:#04X} X:{:#04X} Y:{:#04X} SP:{:#04X} P:{:?} - ", self.pc, self.a, self.x, self.y, self.sp, self.status);
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

    fn resolve_address(&mut self, addressing: AddressingMode) -> u16 {
        use AddressingMode::*;
        match addressing {
            Immediate => {
                let address = self.pc;
                self.pc += 1;
                address
            }
            Absolute => self.fetch_word(),
            AbsoluteX => self.fetch_word().wrapping_add(self.x as u16),
            AbsoluteY => self.fetch_word().wrapping_add(self.y as u16),
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
                let base_low = self.fetch_byte();
                let base_high = self.fetch_byte() as u16;
                let low_address = self.read_byte((base_high << 8) | (base_low as u16)) as u16;
                let high_address = self.read_byte((base_high << 8) | (base_low.wrapping_add(1) as u16)) as u16;
                let address = (high_address << 8) | low_address;
                address
            }
            IndexedIndirect => {
                let byte = self.fetch_byte();
                let base_address = byte.wrapping_add(self.x);
                let low = self.read_byte(base_address as u16) as u16;
                let high = self.read_byte(base_address.wrapping_add(1) as u16) as u16;
                let address = (high << 8) | low;
                address
            }
            IndirectIndexed => {
                let byte = self.fetch_byte();
                let low = self.read_byte(byte as u16) as u16;
                let high = self.read_byte(byte.wrapping_add(1) as u16) as u16;
                let address = ((high << 8) | low).wrapping_add(self.y as u16);
                address
            }
            _ => panic!("Invalid operation")
        }
    }

    fn decode_instruction(&mut self, opcode: u8) -> Instruction {
        match opcode {
            // 転送命令 comp
            0xA9 => LDA { addressing: Immediate, cycle: 2 },
            0xA5 => LDA { addressing: Zeropage, cycle: 3 },
            0xB5 => LDA { addressing: ZeropageX, cycle: 4 },
            0xAD => LDA { addressing: Absolute, cycle: 4 },
            0xB9 => LDA { addressing: AbsoluteY, cycle: 4 },
            0xBD => LDA { addressing: AbsoluteX, cycle: 4 },
            0xA1 => LDA { addressing: IndexedIndirect, cycle: 6 },
            0xB1 => LDA { addressing: IndirectIndexed, cycle: 5 },

            0xA2 => LDX { addressing: Immediate, cycle: 2 },
            0xA6 => LDX { addressing: Zeropage, cycle: 3 },
            0xB6 => LDX { addressing: ZeropageY, cycle: 4 },
            0xAE => LDX { addressing: Absolute, cycle: 4 },
            0xBE => LDX { addressing: AbsoluteY, cycle: 4 },

            0xA0 => LDY { addressing: Immediate, cycle: 2 },
            0xA4 => LDY { addressing: Zeropage, cycle: 3 },
            0xB4 => LDY { addressing: ZeropageX, cycle: 4 },
            0xAC => LDY { addressing: Absolute, cycle: 4 },
            0xBC => LDY { addressing: AbsoluteX, cycle: 4 },

            0x85 => STA { addressing: Zeropage, cycle: 3 },
            0x95 => STA { addressing: ZeropageX, cycle: 4 },
            0x8D => STA { addressing: Absolute, cycle: 4 },
            0x9D => STA { addressing: AbsoluteX, cycle: 5 },
            0x99 => STA { addressing: AbsoluteY, cycle: 5 },
            0x81 => STA { addressing: IndexedIndirect, cycle: 6 },
            0x91 => STA { addressing: IndirectIndexed, cycle: 6 },

            0x86 => STX { addressing: Zeropage, cycle: 3 },
            0x96 => STX { addressing: ZeropageY, cycle: 4 },
            0x8E => STX { addressing: Absolute, cycle: 4 },

            0x84 => STY { addressing: Zeropage, cycle: 3 },
            0x94 => STY { addressing: ZeropageX, cycle: 4 },
            0x8C => STY { addressing: Absolute, cycle: 4 },

            0xAA => TAX { cycle: 2 },
            0xA8 => TAY { cycle: 2 },
            0xBA => TSX { cycle: 2 },
            0x8A => TXA { cycle: 2 },
            0x9A => TXS { cycle: 2 },
            0x98 => TYA { cycle: 2 },

            // 算術命令
            0x69 => ADC { addressing: Immediate, cycle: 2 },
            0x65 => ADC { addressing: Zeropage, cycle: 3 },
            0x75 => ADC { addressing: ZeropageX, cycle: 4 },
            0x6D => ADC { addressing: Absolute, cycle: 4 },
            0x7D => ADC { addressing: AbsoluteX, cycle: 4 },
            0x79 => ADC { addressing: AbsoluteY, cycle: 4 },
            0x61 => ADC { addressing: IndexedIndirect, cycle: 6 },
            0x71 => ADC { addressing: IndirectIndexed, cycle: 5 },

            0x29 => AND { addressing: Immediate, cycle: 2 },
            0x25 => AND { addressing: Zeropage, cycle: 3 },
            0x35 => AND { addressing: ZeropageX, cycle: 4 },
            0x2D => AND { addressing: Absolute, cycle: 4 },
            0x3D => AND { addressing: AbsoluteX, cycle: 4 },
            0x39 => AND { addressing: AbsoluteY, cycle: 4 },
            0x21 => AND { addressing: IndexedIndirect, cycle: 6 },
            0x31 => AND { addressing: IndirectIndexed, cycle: 5 },

            0x0A => ASL { addressing: Accumulator, cycle: 2 },
            0x06 => ASL { addressing: Zeropage, cycle: 5 },
            0x16 => ASL { addressing: ZeropageX, cycle: 6 },
            0x0E => ASL { addressing: Absolute, cycle: 6 },
            0x1E => ASL { addressing: AbsoluteX, cycle: 7 },

            0x24 => BIT { addressing: Zeropage, cycle: 3 },
            0x2C => BIT { addressing: Absolute, cycle: 4 },

            0xC9 => CMP { addressing: Immediate, cycle: 2 },
            0xC5 => CMP { addressing: Zeropage, cycle: 3 },
            0xD5 => CMP { addressing: ZeropageX, cycle: 4 },
            0xCD => CMP { addressing: Absolute, cycle: 4 },
            0xDD => CMP { addressing: AbsoluteX, cycle: 4 },
            0xD9 => CMP { addressing: AbsoluteY, cycle: 4 },
            0xC1 => CMP { addressing: IndexedIndirect, cycle: 6 },
            0xD1 => CMP { addressing: IndirectIndexed, cycle: 5 },

            0xE0 => CPX { addressing: Immediate, cycle: 2 },
            0xE4 => CPX { addressing: Zeropage, cycle: 3 },
            0xEC => CPX { addressing: Absolute, cycle: 4 },

            0xC0 => CPY { addressing: Immediate, cycle: 2 },
            0xC4 => CPY { addressing: Zeropage, cycle: 3 },
            0xCC => CPY { addressing: Absolute, cycle: 4 },

            0xC6 => DEC { addressing: Zeropage, cycle: 5 },
            0xD6 => DEC { addressing: ZeropageX, cycle: 6 },
            0xCE => DEC { addressing: Absolute, cycle: 6 },
            0xDE => DEC { addressing: AbsoluteX, cycle: 7 },

            0xCA => DEX { cycle: 2 },
            0x88 => DEY { cycle: 2 },

            0x49 => EOR { addressing: Immediate, cycle: 2 },
            0x45 => EOR { addressing: Zeropage, cycle: 3 },
            0x55 => EOR { addressing: ZeropageX, cycle: 4 },
            0x4D => EOR { addressing: Absolute, cycle: 4 },
            0x5D => EOR { addressing: AbsoluteX, cycle: 4 },
            0x59 => EOR { addressing: AbsoluteY, cycle: 4 },
            0x41 => EOR { addressing: IndexedIndirect, cycle: 6 },
            0x51 => EOR { addressing: IndirectIndexed, cycle: 5 },

            0xE6 => INC { addressing: Zeropage, cycle: 5 },
            0xF6 => INC { addressing: ZeropageX, cycle: 6 },
            0xEE => INC { addressing: Absolute, cycle: 6 },
            0xFE => INC { addressing: AbsoluteX, cycle: 7 },

            0xE8 => INX { cycle: 2 },

            0xC8 => INY { cycle: 2 },

            0x4A => LSR { addressing: Accumulator, cycle: 2 },
            0x46 => LSR { addressing: Zeropage, cycle: 5 },
            0x56 => LSR { addressing: ZeropageX, cycle: 6 },
            0x4E => LSR { addressing: Absolute, cycle: 6 },
            0x5E => LSR { addressing: AbsoluteX, cycle: 7 },

            0x09 => ORA { addressing: Immediate, cycle: 2 },
            0x05 => ORA { addressing: Zeropage, cycle: 3 },
            0x15 => ORA { addressing: ZeropageX, cycle: 4 },
            0x0D => ORA { addressing: Absolute, cycle: 4 },
            0x1D => ORA { addressing: AbsoluteX, cycle: 4 },
            0x19 => ORA { addressing: AbsoluteY, cycle: 4 },
            0x01 => ORA { addressing: IndexedIndirect, cycle: 6 },
            0x11 => ORA { addressing: IndirectIndexed, cycle: 5 },

            0x2A => ROL { addressing: Accumulator, cycle: 5 },
            0x26 => ROL { addressing: Zeropage, cycle: 5 },
            0x36 => ROL { addressing: ZeropageX, cycle: 6 },
            0x2E => ROL { addressing: Absolute, cycle: 6 },
            0x3E => ROL { addressing: AbsoluteX, cycle: 7 },

            0x6A => ROR { addressing: Accumulator, cycle: 5 },
            0x66 => ROR { addressing: Zeropage, cycle: 5 },
            0x76 => ROR { addressing: ZeropageX, cycle: 6 },
            0x6E => ROR { addressing: Absolute, cycle: 6 },
            0x7E => ROR { addressing: AbsoluteX, cycle: 7 },

            0xE9 => SBC { addressing: Immediate, cycle: 2 },
            0xEB => SBC { addressing: Immediate, cycle: 2 },
            0xE5 => SBC { addressing: Zeropage, cycle: 3 },
            0xF5 => SBC { addressing: ZeropageX, cycle: 4 },
            0xED => SBC { addressing: Absolute, cycle: 4 },
            0xFD => SBC { addressing: AbsoluteX, cycle: 4 },
            0xF9 => SBC { addressing: AbsoluteY, cycle: 4 },
            0xE1 => SBC { addressing: IndexedIndirect, cycle: 6 },
            0xF1 => SBC { addressing: IndirectIndexed, cycle: 5 },

            0x07 => SLO { addressing: Zeropage, cycle: 5 },
            0x17 => SLO { addressing: ZeropageX, cycle: 6 },
            0x0F => SLO { addressing: Absolute, cycle: 6 },
            0x1F => SLO { addressing: AbsoluteX, cycle: 7 },
            0x1B => SLO { addressing: AbsoluteY, cycle: 7 },
            0x03 => SLO { addressing: IndexedIndirect, cycle: 8 },
            0x13 => SLO { addressing: IndirectIndexed, cycle: 8 },

            0x27 => RLA { addressing: Zeropage, cycle: 5 },
            0x37 => RLA { addressing: ZeropageX, cycle: 6 },
            0x2F => RLA { addressing: Absolute, cycle: 6 },
            0x3F => RLA { addressing: AbsoluteX, cycle: 7 },
            0x3B => RLA { addressing: AbsoluteY, cycle: 7 },
            0x23 => RLA { addressing: IndexedIndirect, cycle: 8 },
            0x33 => RLA { addressing: IndirectIndexed, cycle: 8 },

            // スタック命令 comp
            0x48 => PHA { cycle: 3 },
            0x08 => PHP { cycle: 3 },
            0x68 => PLA { cycle: 4 },
            0x28 => PLP { cycle: 4 },

            // ジャンプ命令 comp
            0x4C => JMP { addressing: Absolute, cycle: 3 },
            0x6C => JMP { addressing: Indirect, cycle: 5 },
            0x20 => JSR { cycle: 6 },
            0x60 => RTS { cycle: 6 },
            0x40 => RTI { cycle: 6 },

            // 分岐命令 comp
            0x90 => BCC { cycle: 2 },
            0xB0 => BCS { cycle: 2 },
            0xF0 => BEQ { cycle: 2 },
            0x30 => BMI { cycle: 2 },
            0xD0 => BNE { cycle: 2 },
            0x10 => BPL { cycle: 2 },
            0x50 => BVC { cycle: 2 },
            0x70 => BVS { cycle: 2 },

            // フラグ変更命令 comp
            0x18 => CLC { cycle: 2 },
            0xD8 => CLD { cycle: 2 },
            0x58 => CLI { cycle: 2 },
            0xB8 => CLV { cycle: 2 },
            0x38 => SEC { cycle: 2 },
            0xF8 => SED { cycle: 2 },
            0x78 => SEI { cycle: 2 },

            // その他 comp
            0x00 => BRK { cycle: 7 },

            0x1A => NOP { cycle: 2 },
            0x3A => NOP { cycle: 2 },
            0x5A => NOP { cycle: 2 },
            0x7A => NOP { cycle: 2 },
            0xDA => NOP { cycle: 2 },
            0xEA => NOP { cycle: 2 },
            0xFA => NOP { cycle: 2 },

            0x04 => DOP { addressing: Zeropage, cycle: 3 },
            0x14 => DOP { addressing: ZeropageX, cycle: 4 },
            0x34 => DOP { addressing: ZeropageX, cycle: 4 },
            0x44 => DOP { addressing: Zeropage, cycle: 3 },
            0x54 => DOP { addressing: ZeropageX, cycle: 4 },
            0x64 => DOP { addressing: Zeropage, cycle: 3 },
            0x74 => DOP { addressing: ZeropageX, cycle: 4 },
            0x80 => DOP { addressing: Immediate, cycle: 2 },
            0x82 => DOP { addressing: Immediate, cycle: 2 },
            0x89 => DOP { addressing: Immediate, cycle: 2 },
            0xC2 => DOP { addressing: Immediate, cycle: 2 },
            0xD4 => DOP { addressing: ZeropageX, cycle: 4 },
            0xE2 => DOP { addressing: Immediate, cycle: 2 },
            0xF4 => DOP { addressing: ZeropageX, cycle: 4 },

            0x0C => TOP { addressing: Absolute, cycle: 4 },
            0x1C => TOP { addressing: AbsoluteX, cycle: 4 },
            0x3C => TOP { addressing: AbsoluteX, cycle: 4 },
            0x5C => TOP { addressing: AbsoluteX, cycle: 4 },
            0x7C => TOP { addressing: AbsoluteX, cycle: 4 },
            0xDC => TOP { addressing: AbsoluteX, cycle: 4 },
            0xFC => TOP { addressing: AbsoluteX, cycle: 4 },

            _ => panic!("[CPU] Not implemented opcode: {:#04X}", opcode)
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) -> u16 {
        println!("{:?}", instruction);
        use Instruction::*;
        match instruction {
            LDA { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Absolute |
                    Zeropage |
                    AbsoluteX |
                    AbsoluteY |
                    ZeropageX |
                    ZeropageY |
                    IndirectIndexed |
                    IndexedIndirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.lda(address);
                    }
                    _ => panic!("Invalid Operation LDA addressing: {:?}", addressing)
                }
                cycle
            }
            LDX { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Absolute |
                    AbsoluteY |
                    Zeropage |
                    ZeropageY
                    => {
                        let address = self.resolve_address(addressing);
                        self.ldx(address);
                    }
                    _ => panic!("Invalid Operation LDX addressing: {:?}", addressing)
                }
                cycle
            }
            LDY { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Absolute |
                    AbsoluteX |
                    Zeropage |
                    ZeropageX
                    => {
                        let address = self.resolve_address(addressing);
                        self.ldy(address);
                    }
                    _ => panic!("Invalid Operation LDY addressing: {:?}", addressing)
                }
                cycle
            }
            STA { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndexedIndirect |
                    IndirectIndexed
                    => {
                        let address = self.resolve_address(addressing);
                        self.sta(address);
                    }
                    _ => panic!("Invalid Operation STA addressing: {:?}", addressing)
                }
                cycle
            }
            STX { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    ZeropageY |
                    Absolute
                    => {
                        let address = self.resolve_address(addressing);
                        self.stx(address);
                    }
                    _ => panic!("Invalid Operation STX addressing: {:?}", addressing)
                }
                cycle
            }
            STY { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute
                    => {
                        let address = self.resolve_address(addressing);
                        self.sty(address);
                    }
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
            ADC { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndirectIndexed |
                    IndexedIndirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.adc(address);
                    }
                    _ => panic!("Invalid Operation ADC addressing: {:?}", addressing)
                }
                cycle
            }
            AND { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndirectIndexed |
                    IndexedIndirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.and(address);
                    }
                    _ => panic!("Invalid Operation AND addressing: {:?}", addressing)
                }
                cycle
            }
            ASL { addressing, cycle, .. } => {
                match addressing {
                    Accumulator => self.asl_accumulator(),
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.asl(address);
                    }
                    _ => panic!("Invalid Operation ASL addressing: {:?}", addressing)
                }
                cycle
            }
            BIT { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    Absolute
                    => {
                        let address = self.resolve_address(addressing);
                        self.bit(address);
                    }
                    _ => panic!("Invalid Operation BIT addressing: {:?}", addressing)
                }
                cycle
            }
            CMP { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndirectIndexed |
                    IndexedIndirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.cmp(address);
                    }
                    _ => panic!("Invalid Operation CMP addressing: {:?}", addressing)
                }
                cycle
            }
            CPX { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    Absolute
                    => {
                        let address = self.resolve_address(addressing);
                        self.cpx(address);
                    }
                    _ => panic!("Invalid Operation CPX addressing: {:?}", addressing)
                }
                cycle
            }
            CPY { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    Absolute
                    => {
                        let address = self.resolve_address(addressing);
                        self.cpy(address);
                    }
                    _ => panic!("Invalid Operation CPY addressing: {:?}", addressing)
                }
                cycle
            }
            DEC { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.dec(address);
                    }
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
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndexedIndirect |
                    IndirectIndexed
                    => {
                        let address = self.resolve_address(addressing);
                        self.eor(address);
                    }
                    _ => panic!("Invalid Operation EOR addressing: {:?}", addressing)
                }
                cycle
            }
            INC { addressing, cycle, .. } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.inc(address);
                    }
                    _ => panic!("Invalid Operation INC addressing: {:?}", addressing)
                }
                cycle
            }
            INX { cycle } => {
                self.inx();
                cycle
            }
            INY { cycle } => {
                self.iny();
                cycle
            }
            LSR { addressing, cycle, .. } => {
                match addressing {
                    Accumulator => self.lsr_accumulator(),
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.lsr(address);
                    }
                    _ => panic!("Invalid Operation LSR addressing: {:?}", addressing)
                }
                cycle
            }
            ORA { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndexedIndirect |
                    IndirectIndexed
                    => {
                        let address = self.resolve_address(addressing);
                        self.ora(address);
                    }
                    _ => panic!("Invalid Operation ORA addressing: {:?}", addressing)
                }
                cycle
            }
            ROL { addressing, cycle, .. } => {
                match addressing {
                    Accumulator => self.rol_accumulator(),
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.rol(address);
                    }
                    _ => panic!("Invalid Operation ROL addressing: {:?}", addressing)
                }
                cycle
            }
            ROR { addressing, cycle, .. } => {
                match addressing {
                    Accumulator => self.ror_accumulator(),
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX
                    => {
                        let address = self.resolve_address(addressing);
                        self.ror(address);
                    }
                    _ => panic!("Invalid Operation ROR addressing: {:?}", addressing)
                }
                cycle
            }
            SBC { addressing, cycle, .. } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndirectIndexed |
                    IndexedIndirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.sbc(address);
                    }
                    _ => panic!("Invalid Operation SBC addressing: {:?}", addressing)
                }
                cycle
            }
            SLO { addressing, cycle } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndexedIndirect |
                    IndirectIndexed
                    => {
                        let address = self.resolve_address(addressing);
                        self.slo(address);
                    }
                    _ => panic!("Invalid Operation SLO addressing: {:?}", addressing)
                }
                cycle
            }
            RLA { addressing, cycle } => {
                match addressing {
                    Zeropage |
                    ZeropageX |
                    Absolute |
                    AbsoluteX |
                    AbsoluteY |
                    IndexedIndirect |
                    IndirectIndexed
                    => {
                        let address = self.resolve_address(addressing);
                        self.rla(address);
                    }
                    _ => panic!("Invalid Operation RLA addressing: {:?}", addressing)
                }
                cycle
            }
            PHA { cycle } => {
                self.pha();
                cycle
            }
            PHP { cycle } => {
                self.php();
                cycle
            }
            PLA { cycle } => {
                self.pla();
                cycle
            }
            PLP { cycle } => {
                self.plp();
                cycle
            }
            JMP { addressing, cycle, .. } => {
                match addressing {
                    Absolute |
                    Indirect
                    => {
                        let address = self.resolve_address(addressing);
                        self.jmp(address);
                    }
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
            BCC { cycle } => {
                self.bcc();
                cycle
            }
            BCS { cycle } => {
                self.bcs();
                cycle
            }
            BEQ { cycle } => {
                self.beq();
                cycle
            }
            BMI { cycle } => {
                self.bmi();
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
            BVC { cycle } => {
                self.bvc();
                cycle
            }
            BVS { cycle } => {
                self.bvs();
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
            NOP { cycle } => {
                cycle
            }
            DOP { addressing, cycle } => {
                match addressing {
                    Immediate |
                    Zeropage |
                    ZeropageX
                    => { self.resolve_address(addressing); }
                    _ => panic!("Invalid Operation DOP addressing: {:?}", addressing)
                }
                cycle
            }
            TOP { addressing, cycle } => {
                match addressing {
                    Absolute |
                    AbsoluteX
                    => { self.resolve_address(addressing); }
                    _ => panic!("Invalid Operation TOP addressing: {:?}", addressing)
                }
                cycle
            }
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
    }

    fn tya(&mut self) {
        self.a = self.y;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a)
    }

    // 算術命令
    fn adc(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let carry: u8 = if self.status.carry == true { 0x01 } else { 0x00 };
        let result = self.a.wrapping_add(byte).wrapping_add(carry);

        if self.a > result {
            self.status.carry = true;
        } else {
            self.status.carry = false;
        }

        // Aレジスタとメモリ値の符号が同じで、かつ結果がAレジスタの符号と反転していた場合はオーバーフローをセットする
        self.status.overflow = if ((self.a ^ byte) & 0x80 == 0x00) && ((self.a ^ result) & 0x80 == 0x80) { true } else { false };
        self.a = result;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn and(&mut self, operand: u16) {
        self.a &= self.read_byte(operand);
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn asl_accumulator(&mut self) {
        self.a = self.asl_calc(self.a);
    }

    fn asl(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = self.asl_calc(byte);
        self.write_byte(operand, result);
    }

    fn asl_calc(&mut self, target_byte: u8) -> u8 {
        // 左シフトなので計算対象のbit7が1ならば、桁上がりが発生したと言える
        self.status.carry = if target_byte & 0x80 == 0x80 { true } else { false };
        let result = target_byte << 1;
        self.update_zero_flag(result);
        self.update_negative_flag(result);
        result
    }

    fn bit(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        // ネガティブフラグはbyteのbit7をストアする
        self.status.negative = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
        // オーバーフローフラグはbyteのbit6をストアする
        self.status.overflow = if byte & 0b0100_0000 == 0b0100_0000 { true } else { false };
        // ゼロフラグはAとのビット演算の結果、ゼロならばセットする
        let result = self.a & byte;
        self.update_zero_flag(result);
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
        let result = target_register.wrapping_sub(byte);

        self.update_negative_flag(result);
        self.update_zero_flag(result);

        // 桁下がりがないときはキャリーをセット
        // 桁下がりがあるときはキャリーをクリア
        // targetのbit7とresultのbit7の　XORが1ならばキャリーをクリア
        // TODO もう少し賢くできないものか
        let result_carry = (target_register as i16) - (byte as i16);
        if result_carry >= 0 {
            self.status.carry = true
        } else {
            self.status.carry = false
        }
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

    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.update_negative_flag(self.y);
        self.update_zero_flag(self.y);
    }

    fn lsr_accumulator(&mut self) {
        self.a = self.lsr_calc(self.a);
    }

    fn lsr(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = self.lsr_calc(byte);
        self.write_byte(operand, result);
    }

    fn lsr_calc(&mut self, target_byte: u8) -> u8 {
        self.status.carry = if target_byte & 0x01 == 0x01 { true } else { false };
        let result = target_byte >> 1;
        self.update_zero_flag(result);
        self.update_negative_flag(result);
        result
    }

    fn ora(&mut self, operand: u16) {
        self.a |= self.read_byte(operand);
        self.update_zero_flag(self.a);
        self.update_negative_flag(self.a);
    }

    fn rol_accumulator(&mut self) {
        self.a = self.rol_calc(self.a);
    }

    fn rol(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = self.rol_calc(byte);
        self.write_byte(operand, result);
    }

    fn rol_calc(&mut self, target_byte: u8) -> u8 {
        let after_carry_flag = if target_byte & 0x80 == 0x80 { true } else { false };
        let result = (target_byte << 1) | if self.status.carry == true { 0x01 } else { 0x00 };
        self.update_zero_flag(result);
        self.update_negative_flag(result);
        self.status.carry = after_carry_flag;
        result
    }

    fn ror_accumulator(&mut self) {
        let result = self.ror_calc(self.a);
        self.a = result;
    }

    fn ror(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let result = self.ror_calc(byte);
        self.write_byte(operand, result);
    }

    fn ror_calc(&mut self, target_byte: u8) -> u8 {
        let after_carry_flag = if target_byte & 0x01 == 0x01 { true } else { false };
        let result = (target_byte >> 1) | if self.status.carry == true { 0x80 } else { 0x00 };
        self.update_zero_flag(result);
        self.update_negative_flag(result);
        self.status.carry = after_carry_flag;
        result
    }

    fn sbc(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        let carry: u8 = if self.status.carry == true { 0x00 } else { 0x01 };
        let result = self.a.wrapping_sub(byte).wrapping_sub(carry);

        // 桁下がりがないときはキャリーをセット
        // 桁下がりがあるときはキャリーをクリア
        // TODO もうすこし賢く
        let result_carry = (self.a as i16) - (byte as i16) - (carry as i16);
        if result_carry >= 0 {
            self.status.carry = true;
        } else {
            self.status.carry = false;
        }

        // Aレジスタとメモリ値の符号が異なり、かつ結果がAレジスタの符号と反転していた場合はオーバーフローをセットする
        self.status.overflow = if ((self.a ^ byte) & 0x80 == 0x80) && ((self.a ^ result) & 0x80 == 0x80) { true } else { false };
        self.a = result;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn slo(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        self.a = self.a | (byte << 1);
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
        self.status.carry = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
    }

    fn rla(&mut self, operand: u16) {
        let byte = self.read_byte(operand);
        self.a = ((byte << 1) | if self.status.carry == true { 1 } else { 0 }) & self.a;
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
        self.status.carry = if byte & 0b1000_0000 == 0b1000_0000 { true } else { false };
    }
    // スタック命令
    fn pha(&mut self) {
        self.push_to_stack(self.a);
    }

    fn php(&mut self) {
        self.push_status_to_stack();
    }

    fn pla(&mut self) {
        self.a = self.pop_from_stack();
        self.update_negative_flag(self.a);
        self.update_zero_flag(self.a);
    }

    fn plp(&mut self) {
        self.pop_status_from_stack();
        self.status.unused = true;
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
        self.status.unused = true;
        let low = self.pop_from_stack() as u16;
        let high = self.pop_from_stack() as u16;
        self.pc = (high << 8) | low;
    }

    // 分岐命令
    fn bcc(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.carry == false {
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

    fn bcs(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.carry == true {
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

    fn bmi(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.negative == true {
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

    fn bvc(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.overflow == false {
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

    fn bvs(&mut self) {
        let mut offset = self.fetch_byte();

        if self.status.overflow == true {
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
        self.status.negative = if (calculation_result & 0b1000_0000) == 0b1000_0000 { true } else { false };
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
