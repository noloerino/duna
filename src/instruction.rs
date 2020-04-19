use std::cmp::{max, min};
use std::ops::Add;

#[derive(Debug, Copy, Clone)]
/// A bit vector that can fit inside 32 bits. Used to represent instruction fields.
pub struct BitStr32 {
    pub value: u32,
    pub len: u8,
}

impl BitStr32 {
    fn new(value: u32, len: u8) -> BitStr32 {
        BitStr32 {
            // Mask off upper bits
            value: value & ((1 << len as u32) - 1),
            len,
        }
    }

    fn concat(self, o: BitStr32) -> BitStr32 {
        BitStr32::new((self << o.len) | o.value, self.len + o.len)
    }

    /// Extracts a the bits between start and end, inclusive.
    fn slice(self, start: u8, end: u8) -> BitStr32 {
        let high = max(start, end);
        let low = min(start, end);
        let len = high - low + 1;
        BitStr32::new(self.value >> low as u32, len)
    }

    /// Extracts the bit at index i.
    fn index(self, i: u8) -> BitStr32 {
        BitStr32::new(self.value >> i as u32, 1)
    }
}

impl Add for BitStr32 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        self.concat(other)
    }
}

pub enum InstType {
    R {
        funct7: BitStr32,
    }, I, S, B, U, J, Environ
}

pub trait AbstractInstruction {
    // TODO add method to act on state
    fn inst_type(&self) -> InstType;

    fn opcode(&self) -> BitStr32 {
        let val = match self.inst_type() {
            InstType::R => 0b0110011,
            InstType::I => 0b0000011,
            InstType::Environ => 0b1110011,
            InstType::S => 0b0100011,
            InstType::B => 0b1100011,
            InstType::U => 0b0010111,
            InstType::J => 0b1101111,
        };
        BitStr32::new(val, 7)
    }

    fn to_int(&self) -> u32 {
        match self.inst_type() {
            InstType::R => self.funct7() + self.rs2() + self.rs1() + self.funct3() + self.rd() + self.opcode(),
            InstType::I | InstType::Environ => self.imm() + self.rs1() + self.funct3() + self.rd() + self.opcode(),
            InstType::S => self.imm().slice(11, 5) + self.rs2() + self.rs1() + self.funct3() + self.imm().slice(4, 0) + self.opcode(),
            InstType::B => {
                let imm = self.imm();
                imm.index(12) + imm.slice(10, 5) + self.rs2() + self.rs1() + self.funct3() + imm.slice(4, 1) + imm.index(11) + self.opcode()
            },
            InstType::U => self.imm().slice(31, 12) + self.rd() + self.opcode()
            InstType::J => self.imm().slice(31, 12) + self.rd() + self.opcode()
        }.value
    }
}

pub trait RType : AbstractInstruction {
    // TODO add new(rd, rs1, rs2)
    fn inst_type(&self) -> InstType { InstType::R }
}

pub trait IType : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::I }
}

pub trait EnvironInst : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::Environ }
}

pub trait SType : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::S }
}

pub trait BType : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::B }
    fn opcode(&self) -> BitStr32 { BitStr32::new(0b1100011, 7) }
}

pub trait UType : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::U }
}

pub trait JType : AbstractInstruction {
    fn inst_type(&self) -> InstType { InstType::J }
}
