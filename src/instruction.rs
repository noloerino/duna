use crate::program_state::*;
use std::fmt;

pub struct RInstFields {
    pub funct7: BitStr32,
    pub funct3: BitStr32,
    pub opcode: BitStr32,
}
pub struct IInstFields {
    pub funct3: BitStr32,
    pub opcode: BitStr32,
}
pub struct SInstFields {
    pub funct3: BitStr32,
    pub opcode: BitStr32,
}
pub struct BInstFields {
    pub opcode: BitStr32,
    pub funct3: BitStr32,
}
pub struct UInstFields {
    pub opcode: BitStr32,
}
pub struct JInstFields {
    pub opcode: BitStr32,
}
#[allow(dead_code)]
pub struct EnvironInstFields {
    pub funct3: BitStr32,
    pub opcode: BitStr32,
}

pub struct ConcreteInst {
    pub eval: Box<dyn Fn(&ProgramState) -> StateChange>,
    data: ConcreteInstData,
}

enum ConcreteInstData {
    R {
        fields: RInstFields,
        rd: IRegister,
        rs1: IRegister,
        rs2: IRegister,
    },
    I {
        fields: IInstFields,
        rd: IRegister,
        rs1: IRegister,
        imm: BitStr32,
    },
    S {
        fields: SInstFields,
        rs1: IRegister,
        rs2: IRegister,
        imm: BitStr32,
    },
    B {
        fields: BInstFields,
        rs1: IRegister,
        rs2: IRegister,
        imm: BitStr32,
    },
    U {
        fields: UInstFields,
        rd: IRegister,
        imm: BitStr32,
    },
    J {
        fields: JInstFields,
        rd: IRegister,
        imm: BitStr32,
    },
}

impl ConcreteInst {
    pub fn to_machine_code(&self) -> u32 {
        match self.data {
            ConcreteInstData::R {
                fields:
                    RInstFields {
                        funct7,
                        funct3,
                        opcode,
                    },
                rd,
                rs1,
                rs2,
            } => funct7 + rs2.to_bit_str() + rs1.to_bit_str() + funct3 + rd.to_bit_str() + opcode,
            ConcreteInstData::I {
                fields: IInstFields { funct3, opcode },
                imm,
                rd,
                rs1,
            } => imm + rs1.to_bit_str() + funct3 + rd.to_bit_str() + opcode,
            ConcreteInstData::S {
                fields: SInstFields { funct3, opcode },
                imm,
                rs1,
                rs2,
            } => {
                imm.slice(11, 5)
                    + rs2.to_bit_str()
                    + rs1.to_bit_str()
                    + funct3
                    + imm.slice(4, 0)
                    + opcode
            }
            ConcreteInstData::B {
                fields: BInstFields { funct3, opcode },
                imm,
                rs1,
                rs2,
            } => {
                imm.index(12)
                    + imm.slice(10, 5)
                    + rs2.to_bit_str()
                    + rs1.to_bit_str()
                    + funct3
                    + imm.slice(4, 1)
                    + imm.index(11)
                    + opcode
            }
            ConcreteInstData::U {
                fields: UInstFields { opcode },
                imm,
                rd,
                ..
            } => {
                // don't slice because when we constructed the imm we already truncated it
                imm + rd.to_bit_str() + opcode
            }
            ConcreteInstData::J {
                fields: JInstFields { opcode },
                imm,
                rd,
            } => {
                imm.index(20)
                    + imm.slice(10, 1)
                    + imm.index(11)
                    + imm.slice(19, 12)
                    + rd.to_bit_str()
                    + opcode
            } // TODO implement ecall/ebreak
        }
        .as_u32()
    }
}

impl PartialEq<ConcreteInst> for ConcreteInst {
    fn eq(&self, other: &ConcreteInst) -> bool {
        self.to_machine_code() == other.to_machine_code()
    }
}

impl fmt::Debug for ConcreteInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010X}", self.to_machine_code())
    }
}

pub trait RType {
    fn new(rd: IRegister, rs1: IRegister, rs2: IRegister) -> ConcreteInst {
        ConcreteInst {
            eval: Box::new(move |state| {
                let new_rd_val = Self::eval(state.regfile.read(rs1), state.regfile.read(rs2));
                StateChange::reg_write_pc_p4(state, rd, new_rd_val)
            }),
            data: ConcreteInstData::R {
                fields: Self::inst_fields(),
                rd,
                rs1,
                rs2,
            },
        }
    }

    fn inst_fields() -> RInstFields;

    /// Calculates the new value of rd given values of rs1 and rs2.
    fn eval(rs1_val: DataWord, rs2_val: DataWord) -> DataWord;
}

pub trait IType {
    fn new(rd: IRegister, rs1: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(12);
        ConcreteInst {
            eval: Box::new(move |state| Self::eval(state, rd, rs1, imm_vec)),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> IInstFields;
    fn eval(state: &ProgramState, rd: IRegister, rs1: IRegister, imm: BitStr32) -> StateChange;
}

pub trait ITypeArith {
    fn new(rd: IRegister, rs1: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(12);
        ConcreteInst {
            eval: Box::new(move |state| {
                let new_rd_val = Self::eval(state.regfile.read(rs1), imm_vec);
                StateChange::reg_write_pc_p4(state, rd, new_rd_val)
            }),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> IInstFields;

    /// Calculates the new value of rd given values of rs1 and imm.
    fn eval(rs1_val: DataWord, imm: BitStr32) -> DataWord;
}

pub trait ITypeLoad {
    fn new(rd: IRegister, rs1: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(12);
        ConcreteInst {
            eval: Box::new(move |state| {
                let offs = imm_vec.to_sgn_data_word();
                let rs1_val = state.regfile.read(rs1);
                let addr = DataWord::from(if i32::from(offs) >= 0 {
                    u32::from(rs1_val).wrapping_add(u32::from(offs))
                } else {
                    u32::from(rs1_val).wrapping_sub(u32::from(offs.neg()))
                });
                let new_rd_val = Self::eval(&state.memory, ByteAddress::from(addr));
                StateChange::reg_write_pc_p4(state, rd, new_rd_val)
            }),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> IInstFields;

    /// Calculates the new value of rd given the memory address to read from.
    fn eval(mem: &Memory, addr: ByteAddress) -> DataWord;
}

// pub trait EnvironInst {}

pub trait SType {
    fn new(rs1: IRegister, rs2: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(12);
        ConcreteInst {
            eval: Box::new(move |state| Self::eval(state, rs1, rs2, imm_vec)),
            data: ConcreteInstData::S {
                fields: Self::inst_fields(),
                rs1,
                rs2,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> SInstFields;
    fn eval(state: &ProgramState, rs1: IRegister, rs2: IRegister, imm: BitStr32) -> StateChange;
}

pub trait BType {
    fn new(rs1: IRegister, rs2: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(13);
        ConcreteInst {
            eval: Box::new(move |state| {
                if Self::eval(state.regfile.read(rs1), state.regfile.read(rs2)) {
                    StateChange::pc_update_op(
                        state,
                        ByteAddress::from(i32::from(state.pc).wrapping_add(imm_vec.as_i32())),
                    )
                } else {
                    StateChange::noop(state)
                }
            }),
            data: ConcreteInstData::B {
                fields: Self::inst_fields(),
                rs1,
                rs2,
                imm: imm_vec, // chopping LSB is deferred to to_machine_code
            },
        }
    }

    fn inst_fields() -> BInstFields;

    /// Returns true if the branch should be taken.
    fn eval(rs1_val: DataWord, rs2_val: DataWord) -> bool;
}

pub trait UType {
    fn new(rd: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(20);
        ConcreteInst {
            eval: Box::new(move |state| Self::eval(state, rd, imm_vec)),
            data: ConcreteInstData::U {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec,
            },
        }
    }

    fn inst_fields() -> UInstFields;
    fn eval(state: &ProgramState, rd: IRegister, imm: BitStr32) -> StateChange;
}

pub trait JType {
    fn new(rd: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(20);
        ConcreteInst {
            eval: Box::new(move |state| Self::eval(state, rd, imm_vec)),
            data: ConcreteInstData::J {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec, // chopping LSB is deferred to to_machine_code
            },
        }
    }
    fn inst_fields() -> JInstFields;
    fn eval(state: &ProgramState, rd: IRegister, imm: BitStr32) -> StateChange;
}
