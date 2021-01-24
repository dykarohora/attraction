use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum Addressing {
    Immediate,
    Absolute,
    Zeropage,
    AbsoluteX,
    AbsoluteY,
    ZeropageX,
    ZeropageY,
    IndirectIndexed,
}

#[derive(Debug)]
pub enum Instruction {
    // 転送命令
    LDA { operand: u16, addressing: Addressing, cycle: u16 },
    LDX { operand: u16, addressing: Addressing, cycle: u16 },
    LDY { operand: u16, addressing: Addressing, cycle: u16 },
    STA { operand: u16, addressing: Addressing, cycle: u16 },
    STX { operand: u16, addressing: Addressing, cycle: u16 },
    STY { operand: u16, addressing: Addressing, cycle: u16 },
    TAX { cycle: u16 },
    TAY { cycle: u16 },
    TSX { cycle: u16 },
    TXA { cycle: u16 },
    TXS { cycle: u16 },
    TYA { cycle: u16 },

    // 算術命令
    ADC { operand: u16, addressing: Addressing, cycle: u16 },
    AND { operand: u16, addressing: Addressing, cycle: u16 },
    ASL { operand: u16, addressing: Addressing, cycle: u16 },

    CMP { operand: u16, addressing: Addressing, cycle: u16 },
    CPX { operand: u16, addressing: Addressing, cycle: u16 },
    CPY { operand: u16, addressing: Addressing, cycle: u16 },

    DEC { operand: u16, addressing: Addressing, cycle: u16 },
    DEX { cycle: u16 },
    DEY { cycle: u16 },

    INC { operand: u16, addressing: Addressing, cycle: u16 },
    INX { cycle: u16 },
    INY { cycle: u16 },

    // スタック命令

    // ジャンプ命令
    JMP { operand: u16, addressing: Addressing, cycle: u16 },

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

