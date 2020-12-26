#![allow(clippy::new_ret_no_self)]
use super::arch::*;
use super::registers::RiscVRegister;
use crate::instruction::ConcreteInst;
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

pub type InstApplyFn<S> = dyn Fn(&ProgramState<RiscV<S>, S>) -> InstResult<RiscV<S>, S>;

pub struct RiscVInst<S: AtLeast32b> {
    pub eval: Box<InstApplyFn<S>>,
    data: RiscVInstData,
}

enum RiscVInstData {
    R {
        fields: RInstFields,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
    },
    I {
        fields: IInstFields,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    },
    S {
        fields: SInstFields,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    },
    B {
        fields: BInstFields,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    },
    U {
        fields: UInstFields,
        rd: RiscVRegister,
        imm: BitStr32,
    },
    J {
        fields: JInstFields,
        rd: RiscVRegister,
        imm: BitStr32,
    },
}

impl<S: AtLeast32b> ConcreteInst<RiscV<S>, S> for RiscVInst<S> {
    fn to_machine_code(&self) -> u32 {
        match self.data {
            RiscVInstData::R {
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
            RiscVInstData::I {
                fields: IInstFields { funct3, opcode },
                imm,
                rd,
                rs1,
            } => imm + rs1.to_bit_str() + funct3 + rd.to_bit_str() + opcode,
            RiscVInstData::S {
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
            RiscVInstData::B {
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
            RiscVInstData::U {
                fields: UInstFields { opcode },
                imm,
                rd,
                ..
            } => {
                // don't slice because when we constructed the imm we already truncated it
                imm + rd.to_bit_str() + opcode
            }
            RiscVInstData::J {
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

    fn apply(&self, state: &ProgramState<RiscV<S>, S>) -> InstResult<RiscV<S>, S> {
        (*self.eval)(state)
    }
}

impl<S: AtLeast32b> PartialEq<RiscVInst<S>> for RiscVInst<S> {
    fn eq(&self, other: &RiscVInst<S>) -> bool {
        self.to_machine_code() == other.to_machine_code()
    }
}

impl<S: AtLeast32b> fmt::Debug for RiscVInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010X}", self.to_machine_code())
    }
}

impl<S: AtLeast32b> fmt::Display for RiscVInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RiscVInstData::*;
        let args = match self.data {
            R { rd, rs1, rs2, .. } => format!("{}, {}, {}", rd, rs1, rs2),
            I { rd, rs1, imm, .. } => format!("{}, {}, {}", rd, rs1, i32::from(imm)),
            S { rs1, rs2, imm, .. } => format!("{}, {}({})", rs2, i32::from(imm), rs1),
            B { rs1, rs2, imm, .. } => format!("{}, {}, {}", rs1, rs2, i32::from(imm)),
            U { rd, imm, .. } | J { rd, imm, .. } => format!("{}, {}", rd, i32::from(imm)),
        };
        write!(
            f,
            "<no instruction name> {} | hex {}",
            args,
            self.to_machine_code()
        )
    }
}

pub trait RType<S: AtLeast32b> {
    fn new(rd: RiscVRegister, rs1: RiscVRegister, rs2: RiscVRegister) -> RiscVInst<S> {
        RiscVInst {
            eval: Box::new(move |state| {
                let user_state = &state.user_state;
                let new_rd_val =
                    Self::eval(user_state.regfile.read(rs1), user_state.regfile.read(rs2));
                Ok(UserDiff::reg_write_pc_p4(user_state, rd, new_rd_val))
            }),
            data: RiscVInstData::R {
                fields: Self::inst_fields(),
                rd,
                rs1,
                rs2,
            },
        }
    }

    fn inst_fields() -> RInstFields;

    /// Calculates the new value of rd given values of rs1 and rs2.
    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S>;
}

pub trait IType<S: AtLeast32b> {
    /// Creates an instance of the IType istruction.
    fn new(rd: RiscVRegister, rs1: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = imm.to_bit_str(12);
        RiscVInst {
            eval: Box::new(move |state| <Self as IType<S>>::eval(&state, rd, rs1, imm_vec)),
            data: RiscVInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> IInstFields;
    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S>;
}

pub(crate) trait ITypeArith<S: AtLeast32b>: IType<S> {
    fn inst_fields() -> IInstFields;
    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S>;
}

pub(crate) type MemReadResult<S> = (RegValue<S>, DiffStack<RiscV<S>, S>);

pub(crate) trait ITypeLoad<S: AtLeast32b>: IType<S> {
    fn inst_fields() -> IInstFields;
    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>>;
}

/// Shift instructions have truncated immediates
pub trait ITypeShift<S: AtLeast32b> {
    fn new(rd: RiscVRegister, rs1: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = <Self as ITypeShift<S>>::f7().concat(imm.to_bit_str(5));
        RiscVInst {
            eval: Box::new(move |state| {
                let new_rd_val =
                    <Self as ITypeShift<S>>::eval(state.user_state.regfile.read(rs1), imm_vec);
                Ok(UserDiff::reg_write_pc_p4(&state.user_state, rd, new_rd_val))
            }),
            data: RiscVInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm: imm_vec,
            },
        }
    }
    fn f7() -> BitStr32;
    fn inst_fields() -> IInstFields;
    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S>;
}

pub trait EnvironInst<S: AtLeast32b> {
    fn new() -> RiscVInst<S> {
        RiscVInst {
            eval: Box::new(|state| Self::eval(state)),
            data: RiscVInstData::I {
                fields: Self::inst_fields(),
                rd: RiscVRegister::ZERO,
                rs1: RiscVRegister::ZERO,
                imm: Self::funct12(),
            },
        }
    }
    fn funct12() -> BitStr32;
    fn inst_fields() -> IInstFields;
    fn eval(state: &ProgramState<RiscV<S>, S>) -> InstResult<RiscV<S>, S>;
}

pub trait SType<S: AtLeast32b> {
    fn new(rs1: RiscVRegister, rs2: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = imm.to_bit_str(12);
        RiscVInst {
            eval: Box::new(move |state| Self::eval(&state, rs1, rs2, imm_vec)),
            data: RiscVInstData::S {
                fields: Self::inst_fields(),
                rs1,
                rs2,
                imm: imm_vec,
            },
        }
    }
    fn inst_fields() -> SInstFields;
    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S>;
}

pub trait BType<S: AtLeast32b> {
    fn new(rs1: RiscVRegister, rs2: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = imm.to_bit_str(13);
        RiscVInst {
            eval: Box::new(move |state| {
                let user_state = &state.user_state;
                if Self::eval(user_state.regfile.read(rs1), user_state.regfile.read(rs2)) {
                    let pc: SignedValue<S> = user_state.pc.into();
                    let offs: SignedValue<S> = imm_vec.into();
                    let new_pc: SignedValue<S> = pc + offs;
                    Ok(UserDiff::pc_update_op(user_state, new_pc.into()))
                } else {
                    Ok(UserDiff::pc_p4(user_state).into_diff_stack())
                }
            }),
            data: RiscVInstData::B {
                fields: Self::inst_fields(),
                rs1,
                rs2,
                imm: imm_vec, // chopping LSB is deferred to to_machine_code
            },
        }
    }

    fn inst_fields() -> BInstFields;

    /// Returns true if the branch should be taken.
    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool;
}

pub trait UType<S: AtLeast32b> {
    fn new(rd: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = imm.to_bit_str(20);
        RiscVInst {
            eval: Box::new(move |state| Ok(Self::eval(&state.user_state, rd, imm_vec))),
            data: RiscVInstData::U {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec,
            },
        }
    }

    fn inst_fields() -> UInstFields;
    fn eval(
        state: &UserState<RiscV<S>, S>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> DiffStack<RiscV<S>, S>;
}

pub trait JType<S: AtLeast32b> {
    fn new(rd: RiscVRegister, imm: RegValue<S>) -> RiscVInst<S> {
        let imm_vec = imm.to_bit_str(20);
        RiscVInst {
            eval: Box::new(move |state| Ok(Self::eval(&state.user_state, rd, imm_vec))),
            data: RiscVInstData::J {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec, // chopping LSB is deferred to to_machine_code
            },
        }
    }
    fn inst_fields() -> JInstFields;
    fn eval(
        state: &UserState<RiscV<S>, S>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> DiffStack<RiscV<S>, S>;
}
