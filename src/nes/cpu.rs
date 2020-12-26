pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    p: u8
}

pub fn print() {
    println!("hello, module");
}