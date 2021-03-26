#![allow(clippy::new_ret_no_self)]
use super::{arch::Mips, exception::Exception, program::MipsCsr, registers::MipsRegister};
use crate::{data_structures::*, instruction::*, program_state::*};
use std::fmt;

pub type InstApplyFn<S> = dyn Fn(&ProgramState<Mips<S>, S>) -> InstResult<Mips<S>, S>;

pub struct MipsInst<S: AtLeast32b> {
    pub eval: Box<InstApplyFn<S>>,
    data: InstData,
}

struct InstData {
    name: &'static str,
    fields: InstFields,
}

impl InstData {
    fn new(name: &'static str, fields: InstFields) -> Self {
        Self { name, fields }
    }
}

enum InstFields {
    R {
        rd: MipsRegister,
        rs: MipsRegister,
        rt: MipsRegister,
        fields: RInstFields,
    },
    I {
        opcode: BitStr32,
        rs: MipsRegister,
        rt: MipsRegister,
        imm: BitStr32,
    },
    J {
        opcode: BitStr32,
        addr: BitStr32,
    },
}

impl<S: AtLeast32b> ConcreteInst<Mips<S>, S> for MipsInst<S> {
    fn to_machine_code(&self) -> u32 {
        match self.data.fields {
            InstFields::R {
                rd,
                rs,
                rt,
                fields:
                    RInstFields {
                        opcode,
                        shamt,
                        funct,
                    },
            } => opcode + rs.to_bit_str() + rt.to_bit_str() + rd.to_bit_str() + shamt + funct,
            InstFields::I {
                opcode,
                rs,
                rt,
                imm,
            } => opcode + rs.to_bit_str() + rt.to_bit_str() + imm,
            InstFields::J { opcode, addr } => opcode + addr,
        }
        .as_u32()
    }

    fn apply(&self, state: &ProgramState<Mips<S>, S>) -> InstResult<Mips<S>, S> {
        (*self.eval)(state)
    }
}

impl<S: AtLeast32b> PartialEq<MipsInst<S>> for MipsInst<S> {
    fn eq(&self, other: &MipsInst<S>) -> bool {
        self.to_machine_code() == other.to_machine_code()
    }
}

impl<S: AtLeast32b> fmt::Debug for MipsInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<S: AtLeast32b> fmt::UpperHex for MipsInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010X}", self.to_machine_code())
    }
}

impl<S: AtLeast32b> fmt::LowerHex for MipsInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010x}", self.to_machine_code())
    }
}

impl<S: AtLeast32b> fmt::Display for MipsInst<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // use InstFields::*;
        write!(f, "{} unimplemented", self.data.name)
        // let args = match self.data.fields {
        //     R { rd, rs, rt, .. } => format!("{}, {}, {}", rd, rs1, rs2),
        //     I { rs, rt, imm, .. } => format!("{}, {}, {}", rd, rs1, i32::from(imm)),
        //     J { opcode, addr, .. } => format!("{}, {}({})", rs2, i32::from(imm), rs1),
        // };
        // write!(f, "{} {}", self.data.name, args)
    }
}

pub struct RInstFields {
    pub opcode: BitStr32,
    pub shamt: BitStr32,
    pub funct: BitStr32,
}

pub trait RType<S: AtLeast32b> {
    fn new(rd: MipsRegister, rs: MipsRegister, rt: MipsRegister) -> MipsInst<S> {
        MipsInst {
            eval: Box::new(move |state| {
                let user_state = &state.user_state;
                let priv_state = &state.priv_state;
                let maybe_new_rd_val =
                    Self::eval(user_state.regfile.read(rs), user_state.regfile.read(rt));
                Ok(maybe_new_rd_val
                    .map(|new_rd_val| UserDiff::reg_write_pc_p4(user_state, rd, new_rd_val))
                    .unwrap_or_else(|e| {
                        // TODO find some way to trap to OS on exceptions
                        match e {
                            Exception::Overflow => vec![
                                // Save PC
                                PrivDiff::csr_write(
                                    priv_state,
                                    MipsCsr::Epc as usize,
                                    user_state.pc.plus_4().into(),
                                )
                                .into_state_diff(),
                                // TODO Update status register (see SPIM manual)
                                PrivDiff::csr_write(
                                    priv_state,
                                    MipsCsr::Cause as usize,
                                    RegValue::<S>::from(
                                        // arith overflow is 12; exception code starts at bit 2
                                        (12 << 2) as u64,
                                    ),
                                )
                                .into_state_diff(),
                                // for now, pretend that no interrupt handles integer overflow and
                                // we just go to the next instruction
                                UserDiff::pc_p4(user_state).into_state_diff(),
                            ],
                            // _ => unimplemented!(),
                        }
                    }))
            }),
            data: InstData::new(
                Self::name(),
                InstFields::R {
                    fields: Self::inst_fields(),
                    rd,
                    rs,
                    rt,
                },
            ),
        }
    }

    fn name() -> &'static str;

    fn inst_fields() -> RInstFields;

    // JR is the only R-type that doesn't set rd, so we can just special case that
    /// Calculates the new value of rd given values of rs1 and rs2.
    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> Result<RegValue<S>, Exception>;
}

pub trait IType<S: AtLeast32b> {
    fn new(rs: MipsRegister, rt: MipsRegister, imm: BitStr32) -> MipsInst<S> {
        MipsInst {
            eval: Box::new(move |state| Self::eval(&state.user_state, rs, rt, imm)),
            data: InstData::new(
                Self::name(),
                InstFields::I {
                    opcode: Self::opcode(),
                    rs,
                    rt,
                    imm,
                },
            ),
        }
    }

    fn name() -> &'static str;

    fn opcode() -> BitStr32;

    fn eval(
        user_state: &UserState<Mips<S>, S>,
        rs: MipsRegister,
        rt: MipsRegister,
        imm: BitStr32,
    ) -> InstResult<Mips<S>, S>;
}

pub trait JType<S: AtLeast32b> {
    fn new(addr: BitStr32) -> MipsInst<S> {
        MipsInst {
            eval: Box::new(move |state| Self::eval(&state.user_state, addr)),
            data: InstData::new(
                Self::name(),
                InstFields::J {
                    opcode: Self::opcode(),
                    addr,
                },
            ),
        }
    }

    fn name() -> &'static str;

    fn opcode() -> BitStr32;

    fn eval(user_state: &UserState<Mips<S>, S>, addr: BitStr32) -> InstResult<Mips<S>, S>;
}
