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

pub struct ConcreteInst {
    pub eval: Box<dyn Fn(&ProgramState) -> InstResult>,
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
            }
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
                let user_state = &state.user_state;
                let new_rd_val =
                    Self::eval(user_state.regfile.read(rs1), user_state.regfile.read(rs2));
                UserDiff::reg_write_pc_p4(user_state, rd, new_rd_val).into_inst_result()
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
            // closure must contain match, since otherwise the types for arms don't match
            eval: Box::new(move |state| {
                InstResult::UserStateChange(<Self as IType>::eval(
                    &state.user_state,
                    rd,
                    rs1,
                    imm_vec,
                ))
            }),
            // Box::new(move |user_state| match Self::eval_wrapper() {
            //     Generic(eval) => eval(user_state, rd, rs1, imm_vec),
            //     Arith(eval) => {
            //         let new_rd_val = eval(user_state.regfile.read(rs1), imm_vec);
            //         UserStateChange::reg_write_pc_p4(user_state, rd, new_rd_val)
            //     }
            //     Load(eval) => {
            //         let offs = imm_vec.to_sgn_data_word();
            //         let rs1_val = user_state.regfile.read(rs1);
            //         let addr = DataWord::from(i32::from(rs1_val).wrapping_add(i32::from(offs)));
            //         let new_rd_val = eval(&user_state.memory, ByteAddress::from(addr));
            //         UserStateChange::reg_write_pc_p4(user_state, rd, new_rd_val)
            //     }
            // }),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> IInstFields;
    fn eval(state: &UserProgState, rd: IRegister, rs1: IRegister, imm: BitStr32) -> UserDiff;
}

pub(crate) trait ITypeArith: IType {
    fn inst_fields() -> IInstFields;
    fn eval(rs1_val: DataWord, imm: BitStr32) -> DataWord;
}

pub(crate) trait ITypeLoad: IType {
    fn inst_fields() -> IInstFields;
    fn eval(mem: &Memory, addr: ByteAddress) -> DataWord;
}

pub trait EnvironInst {
    fn new() -> ConcreteInst {
        ConcreteInst {
            eval: Box::new(|state| InstResult::Trap(Self::eval(state))),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd: IRegister::ZERO,
                rs1: IRegister::ZERO,
                imm: Self::funct12(),
            },
        }
    }
    fn funct12() -> BitStr32;
    fn inst_fields() -> IInstFields;
    fn eval(state: &ProgramState) -> TrapKind;
}

pub trait SType {
    fn new(rs1: IRegister, rs2: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(12);
        ConcreteInst {
            eval: Box::new(move |state| {
                InstResult::UserStateChange(Self::eval(&state.user_state, rs1, rs2, imm_vec))
            }),
            data: ConcreteInstData::S {
                fields: Self::inst_fields(),
                rs1,
                rs2,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> SInstFields;
    fn eval(state: &UserProgState, rs1: IRegister, rs2: IRegister, imm: BitStr32) -> UserDiff;
}

pub trait BType {
    fn new(rs1: IRegister, rs2: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(13);
        ConcreteInst {
            eval: Box::new(move |state| {
                let user_state = &state.user_state;
                if Self::eval(user_state.regfile.read(rs1), user_state.regfile.read(rs2)) {
                    UserDiff::pc_update_op(
                        user_state,
                        ByteAddress::from(i32::from(user_state.pc).wrapping_add(imm_vec.as_i32())),
                    )
                } else {
                    UserDiff::noop(user_state)
                }
                .into_inst_result()
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
            eval: Box::new(move |state| {
                InstResult::UserStateChange(Self::eval(&state.user_state, rd, imm_vec))
            }),
            data: ConcreteInstData::U {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec,
            },
        }
    }

    fn inst_fields() -> UInstFields;
    fn eval(state: &UserProgState, rd: IRegister, imm: BitStr32) -> UserDiff;
}

pub trait JType {
    fn new(rd: IRegister, imm: DataWord) -> ConcreteInst {
        let imm_vec = imm.to_bit_str(20);
        ConcreteInst {
            eval: Box::new(move |state| {
                InstResult::UserStateChange(Self::eval(&state.user_state, rd, imm_vec))
            }),
            data: ConcreteInstData::J {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec, // chopping LSB is deferred to to_machine_code
            },
        }
    }
    fn inst_fields() -> JInstFields;
    fn eval(state: &UserProgState, rd: IRegister, imm: BitStr32) -> UserDiff;
}
