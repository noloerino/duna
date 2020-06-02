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

pub struct ConcreteInst<T: MachineDataWidth> {
    pub eval: Box<dyn Fn(&ProgramState<T>) -> InstResult<T>>,
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

impl<T: MachineDataWidth> ConcreteInst<T> {
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

impl<T: MachineDataWidth> PartialEq<ConcreteInst<T>> for ConcreteInst<T> {
    fn eq(&self, other: &ConcreteInst<T>) -> bool {
        self.to_machine_code() == other.to_machine_code()
    }
}

impl<T: MachineDataWidth> fmt::Debug for ConcreteInst<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010X}", self.to_machine_code())
    }
}

impl<T: MachineDataWidth> fmt::Display for ConcreteInst<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ConcreteInstData::*;
        let data = &self.data;
        let args = match data {
            R { rd, rs1, rs2, .. } => format!("{}, {}, {}", rd, rs1, rs2),
            I { rd, rs1, imm, .. } => format!("{}, {}, {}", rd, rs1, imm.as_i32()),
            S { rs1, rs2, imm, .. } => format!("{}, {}({})", rs2, imm.as_i32(), rs1),
            B { rs1, rs2, imm, .. } => format!("{}, {}, {}", rs1, rs2, imm.as_i32()),
            U { rd, imm, .. } | J { rd, imm, .. } => format!("{}, {}", rd, imm.as_i32()),
        };
        write!(
            f,
            "<no instruction name> {} | hex {}",
            args,
            self.to_machine_code()
        )
    }
}

pub trait RType<T: MachineDataWidth> {
    fn new(rd: IRegister, rs1: IRegister, rs2: IRegister) -> ConcreteInst<T> {
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
    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData;
}

pub trait IType<T: MachineDataWidth> {
    /// Creates an instance of the IType istruction.
    fn new(rd: IRegister, rs1: IRegister, imm: T::RegData) -> ConcreteInst<T> {
        ConcreteInst {
            // closure must contain match, since otherwise the types for arms don't match
            eval: Box::new(move |state| {
                InstResult::UserStateChange(<Self as IType<T>>::eval(
                    &state.user_state,
                    rd,
                    rs1,
                    imm,
                ))
            }),
            data: ConcreteInstData::I {
                fields: Self::inst_fields(),
                rd,
                rs1,
                imm,
            },
        }
    }
    fn inst_fields() -> IInstFields;
    fn eval(
        state: &UserProgState<T>,
        rd: IRegister,
        rs1: IRegister,
        imm: T::RegData,
    ) -> UserDiff<T>;
}

pub(crate) trait ITypeArith<T: MachineDataWidth>: IType<T> {
    fn inst_fields() -> IInstFields;
    fn eval(rs1_val: T::RegData, imm: T::RegData) -> T::RegData;
}

pub(crate) trait ITypeLoad<T: MachineDataWidth>: IType<T> {
    fn inst_fields() -> IInstFields;
    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData;
}

pub trait EnvironInst<T: MachineDataWidth> {
    fn new() -> ConcreteInst<T> {
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
    fn eval(state: &ProgramState<T>) -> TrapKind;
}

pub trait SType<T: MachineDataWidth> {
    fn new(rs1: IRegister, rs2: IRegister, imm: T::RegData) -> ConcreteInst<T> {
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
    fn eval(state: &UserProgState<T>, rs1: IRegister, rs2: IRegister, imm: BitStr32)
        -> UserDiff<T>;
}

pub trait BType<T: MachineDataWidth> {
    fn new(rs1: IRegister, rs2: IRegister, imm: T::RegData) -> ConcreteInst<T> {
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
    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool;
}

pub trait UType<T: MachineDataWidth> {
    fn new(rd: IRegister, imm: T::RegData) -> ConcreteInst<T> {
        let imm_vec = imm.to_bit_str(20);
        ConcreteInst {
            eval: Box::new(move |state| {
                InstResult::UserStateChange(Self::eval(&state.user_state, rd, imm))
            }),
            data: ConcreteInstData::U {
                fields: Self::inst_fields(),
                rd,
                imm: imm_vec,
            },
        }
    }

    fn inst_fields() -> UInstFields;
    fn eval(state: &UserProgState<T>, rd: IRegister, imm: T::RegData) -> UserDiff<T>;
}

pub trait JType<T: MachineDataWidth> {
    fn new(rd: IRegister, imm: T::RegData) -> ConcreteInst<T> {
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
    fn eval(state: &UserProgState<T>, rd: IRegister, imm: T::RegData) -> UserDiff<T>;
}
