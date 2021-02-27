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
pub enum Instruction {
    // 転送命令
    LDA { addressing: AddressingMode, cycle: u16 },
    LDX { addressing: AddressingMode, cycle: u16 },
    LDY { addressing: AddressingMode, cycle: u16 },
    STA { addressing: AddressingMode, cycle: u16 },
    STX { addressing: AddressingMode, cycle: u16 },
    STY { addressing: AddressingMode, cycle: u16 },
    TAX { cycle: u16 },
    TAY { cycle: u16 },
    TSX { cycle: u16 },
    TXA { cycle: u16 },
    TXS { cycle: u16 },
    TYA { cycle: u16 },

    // 算術命令
    ADC { addressing: AddressingMode, cycle: u16 },
    AND { addressing: AddressingMode, cycle: u16 },
    ASL { addressing: AddressingMode, cycle: u16 },
    BIT { addressing: AddressingMode, cycle: u16 },

    CMP { addressing: AddressingMode, cycle: u16 },
    CPX { addressing: AddressingMode, cycle: u16 },
    CPY { addressing: AddressingMode, cycle: u16 },

    DEC { addressing: AddressingMode, cycle: u16 },
    DEX { cycle: u16 },
    DEY { cycle: u16 },

    EOR { addressing: AddressingMode, cycle: u16 },

    INC { addressing: AddressingMode, cycle: u16 },
    INX { cycle: u16 },
    INY { cycle: u16 },

    LSR { addressing: AddressingMode, cycle: u16 },
    ORA { addressing: AddressingMode, cycle: u16 },
    ROL { addressing: AddressingMode, cycle: u16 },
    ROR { addressing: AddressingMode, cycle: u16 },
    SBC { addressing: AddressingMode, cycle: u16 },
    SLO { addressing: AddressingMode, cycle: u16 }, // unofficial
    RLA { addressing: AddressingMode, cycle: u16 }, // unofficial

    // スタック命令
    PHA { cycle: u16 },
    PHP { cycle: u16 },
    PLA { cycle: u16 },
    PLP { cycle: u16 },

    // ジャンプ命令
    JMP { addressing: AddressingMode, cycle: u16 },
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

