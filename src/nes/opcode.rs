use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum AddressingMode {
    Accumulator,
    Immediate,
    Absolute,
    Zeropage,
    AbsoluteX,
    AbsoluteY,
    ZeropageX,
    ZeropageY,
    Indirect,
    IndirectIndexed,
    IndexedIndirect,
}

#[derive(Debug)]
pub enum Addressing {
    Accumulator,
    Immediate(u16),
    Absolute(u16),
    Zeropage(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeropageX(u16),
    ZeropageY(u16),
    Indirect(u16),
    IndirectIndexed(u16),
    IndexedIndirect(u16),
}

#[derive(Debug)]
pub enum Instruction {
    // 転送命令
    LDA { addressing: Addressing, cycle: u16 },
    LDX { addressing: Addressing, cycle: u16 },
    LDY { addressing: Addressing, cycle: u16 },
    STA { addressing: Addressing, cycle: u16 },
    STX { addressing: Addressing, cycle: u16 },
    STY { addressing: Addressing, cycle: u16 },
    TAX { cycle: u16 },
    TAY { cycle: u16 },
    TSX { cycle: u16 },
    TXA { cycle: u16 },
    TXS { cycle: u16 },
    TYA { cycle: u16 },

    // 算術命令
    ADC { addressing: Addressing, cycle: u16 },
    AND { addressing: Addressing, cycle: u16 },
    ASL { addressing: Addressing, cycle: u16 },

    CMP { addressing: Addressing, cycle: u16 },
    CPX { addressing: Addressing, cycle: u16 },
    CPY { addressing: Addressing, cycle: u16 },

    DEC { addressing: Addressing, cycle: u16 },
    DEX { cycle: u16 },
    DEY { cycle: u16 },

    EOR { addressing: Addressing, cycle: u16 },

    INC { addressing: Addressing, cycle: u16 },
    INX { cycle: u16 },
    INY { cycle: u16 },

    LSR { addressing: Addressing, cycle: u16 },
    ORA { addressing: Addressing, cycle: u16 },
    ROL { addressing: Addressing, cycle: u16 },
    ROR { addressing: Addressing, cycle: u16 },
    SBC { addressing: Addressing, cycle: u16 },

    // スタック命令
    PHA { cycle: u16 },
    PLA { cycle: u16 },

    // ジャンプ命令
    JMP { addressing: Addressing, cycle: u16 },
    JSR { cycle: u16 },
    RTS { cycle: u16 },
    RTI { cycle: u16 },

    // 分岐命令
    BCC { cycle: u16 },
    BCS { cycle: u16 },
    BEQ { cycle: u16 },
    BMI { cycle: u16 },
    BNE { cycle: u16 },
    BPL { cycle: u16 },
    BVC { cycle: u16 },
    BVS { cycle: u16 },

    // フラグ変更命令
    CLC { cycle: u16 },
    CLD { cycle: u16 },
    CLI { cycle: u16 },
    CLV { cycle: u16 },

    SEC { cycle: u16 },
    SED { cycle: u16 },
    SEI { cycle: u16 },

    // その他
    BRK { cycle: u16 },
    NOP { cycle: u16 },
}

