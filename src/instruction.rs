use crate::program_state::*;

pub enum InstType {
    R { funct7: BitStr32, funct3: BitStr32, rd: IRegister, rs1: IRegister, rs2: IRegister },
    I { funct3: BitStr32, imm: BitStr32, rd: IRegister, rs1: IRegister },
    S { funct3: BitStr32, imm: BitStr32, rs1: IRegister, rs2: IRegister },
    B { funct3: BitStr32, imm: BitStr32, rs1: IRegister, rs2: IRegister },
    U { imm: BitStr32, rd: IRegister },
    J { imm: BitStr32, rd: IRegister },
    Environ { imm: BitStr32, funct3: BitStr32 },
}

pub trait AbstractInstruction {
    // TODO add method to act on state
    fn inst_type(&self) -> InstType;

    fn opcode(&self) -> BitStr32 {
        let val = match self.inst_type() {
            InstType::R { .. } => 0b0110011,
            InstType::I { .. }=> 0b0000011,
            InstType::Environ { .. } => 0b1110011,
            InstType::S { .. } => 0b0100011,
            InstType::B { .. } => 0b1100011,
            InstType::U { .. } => 0b0010111,
            InstType::J { .. } => 0b1101111,
        };
        BitStr32::new(val, 7)
    }

    fn to_int(&self) -> u32 {
        match self.inst_type() {
            InstType::R { funct7, funct3, rd, rs1, rs2 } => {
                funct7 + rs2.to_bit_str() + rs1.to_bit_str() + funct3 + rd.to_bit_str() + self.opcode()
            }
            InstType::I { funct3, imm, rd, rs1 } => {
                imm + rs1.to_bit_str() + funct3 + rd.to_bit_str() + self.opcode()
            }
            InstType::S { funct3, imm, rs1, rs2} => {
                imm.slice(11, 5)
                    + rs2.to_bit_str()
                    + rs1.to_bit_str()
                    + funct3
                    + imm.slice(4, 0)
                    + self.opcode()
            }
            InstType::B { funct3, imm, rs1, rs2 } => {
                imm.index(12)
                    + imm.slice(10, 5)
                    + rs2.to_bit_str()
                    + rs1.to_bit_str()
                    + funct3
                    + imm.slice(4, 1)
                    + imm.index(11)
                    + self.opcode()
            }
            InstType::U { imm, rd} => imm.slice(31, 12) + rd.to_bit_str() + self.opcode(),
            InstType::J { imm, rd} => imm.slice(31, 12) + rd.to_bit_str() + self.opcode(),
            // TODO implement ecall/ebreak
            InstType::Environ { imm, funct3 } => BitStr32::new(0, 32)
        }
        .value
    }
}

pub trait RType: AbstractInstruction {
    // fn new(rd: IRegister, rs1: IRegister, rs2: IRegister) -> Self;
}

pub trait IType: AbstractInstruction {
    // fn new(imm: u32, rs1: IRegister, rs2: IRegister);
}

pub trait EnvironInst: AbstractInstruction {
}

pub trait SType: AbstractInstruction {
}

pub trait BType: AbstractInstruction {
}

pub trait UType: AbstractInstruction {
}

pub trait JType: AbstractInstruction {
}
