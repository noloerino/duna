use crate::{
    architectures::riscv::{
        instruction::*,
        isa::i::{f3, f7},
    },
    data_structures::*,
};
use num_traits::ops::wrapping::{WrappingAdd, WrappingSub};

const R_OPCODE: BitStr32 = BitStr32::new(0b011_0011, 7);
const R_W_OPCODE: BitStr32 = BitStr32::new(0b011_1011, 7);

pub struct Add;
impl<S: AtLeast32b> RType<S> for Add {
    fn name() -> &'static str {
        "add"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        v1.wrapping_add(&rs2_val.as_signed()).as_reg_data()
    }
}

pub struct Addw;
impl RType<W64b> for Addw {
    fn name() -> &'static str {
        "addw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: DataDword, rs2_val: DataDword) -> DataDword {
        let v1: DataLword = rs1_val.lower_lword();
        let v2: DataLword = rs2_val.lower_lword();
        let result: DataLword = u32::from(v1).wrapping_add(u32::from(v2)).into();
        DataDword::sign_ext_from_lword(result)
    }
}

pub struct And;
impl<S: AtLeast32b> RType<S> for And {
    fn name() -> &'static str {
        "and"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b111),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        (v1 & rs2_val.into()).into()
    }
}

pub struct Or;
impl<S: AtLeast32b> RType<S> for Or {
    fn name() -> &'static str {
        "or"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b110),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        (v1 | rs2_val.into()).into()
    }
}

/// If S is W32b, masks lower 5 bits; else lower 6 bits
fn shamt<S: AtLeast32b>(n: RegValue<S>) -> RegValue<S> {
    n & RegValue::<S>::new(<S as DataWidth>::from_u64(if <S as AtLeast32b>::is_32() {
        0b1_1111
    } else {
        0b11_1111
    }))
}

pub struct Sll;
impl<S: AtLeast32b> RType<S> for Sll {
    fn name() -> &'static str {
        "sll"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b001),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: UnsignedValue<S> = rs1_val.into();
        (v1 << shamt(rs2_val).into()).into()
    }
}

pub struct Sllw;
impl RType<W64b> for Sllw {
    fn name() -> &'static str {
        "sllw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b001),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        let v1: SignedValue<W64b> = rs1_val.into();
        // Take only lower 6 bits
        (v1 << shamt(rs2_val).into()).into()
    }
}

pub struct Slt;
impl<S: AtLeast32b> RType<S> for Slt {
    fn name() -> &'static str {
        "slt"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0),
            funct3: f3(0b010),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        if v1 < rs2_val.into() {
            RegValue::<S>::new(S::from_u64(1))
        } else {
            RegValue::<S>::zero()
        }
    }
}

pub struct Sltu;
impl<S: AtLeast32b> RType<S> for Sltu {
    fn name() -> &'static str {
        "sltu"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0),
            funct3: f3(0b011),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: UnsignedValue<S> = rs1_val.into();
        if v1 < rs2_val.into() {
            RegValue::<S>::new(S::from_u64(1))
        } else {
            RegValue::<S>::zero()
        }
    }
}

pub struct Sra;
impl<S: AtLeast32b> RType<S> for Sra {
    fn name() -> &'static str {
        "sra"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0b101),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        (v1 >> shamt(rs2_val).into()).into()
    }
}

pub struct Sraw;
impl RType<W64b> for Sraw {
    fn name() -> &'static str {
        "sraw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0b101),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        let v1: SignedValue<W64b> = rs1_val.into();
        (v1 >> shamt(rs2_val).into()).into()
    }
}

pub struct Srl;
impl<S: AtLeast32b> RType<S> for Srl {
    fn name() -> &'static str {
        "srl"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b101),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: UnsignedValue<S> = rs1_val.into();
        (v1 >> shamt(rs2_val).into()).into()
    }
}

pub struct Srlw;
impl RType<W64b> for Srlw {
    fn name() -> &'static str {
        "srlw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b101),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        let v1: UnsignedValue<W64b> = rs1_val.into();
        (v1 >> shamt(rs2_val).into()).into()
    }
}

pub struct Sub;
impl<S: AtLeast32b> RType<S> for Sub {
    fn name() -> &'static str {
        "sub"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: UnsignedValue<S> = rs1_val.into();
        (v1.wrapping_sub(&rs2_val.into())).into()
    }
}

pub struct Subw;
impl RType<W64b> for Subw {
    fn name() -> &'static str {
        "subw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: DataDword, rs2_val: DataDword) -> DataDword {
        let v1: DataLword = rs1_val.lower_lword();
        let v2: DataLword = rs2_val.lower_lword();
        let result: DataLword = u32::from(v1).wrapping_sub(u32::from(v2)).into();
        DataDword::sign_ext_from_lword(result)
    }
}

pub struct Xor;
impl<S: AtLeast32b> RType<S> for Xor {
    fn name() -> &'static str {
        "xor"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b100),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        (v1 ^ rs2_val.into()).into()
    }
}

#[cfg(test)]
mod tests_32 {
    use super::{super::tests_32::*, *};
    use crate::{architectures::riscv::*, program_state::ProgramState};

    struct RTestData {
        rs2: RiscVRegister,
        result: i32,
    }

    #[test]
    fn test_slt_sltu() {
        use RiscVRegister::*;
        let mut state = get_init_state();
        let rs1_val = DataLword::from(-1i32);
        let rs2_val = DataLword::zero();
        state.regfile_set(S0, rs1_val);
        state.regfile_set(S1, rs2_val);
        state.apply_inst_test(&Slt::new(A0, S0, S1));
        assert_eq!(state.regfile_read(A0), DataLword::from(1i32));
        // Unsigned interpretation makes -1i32 the largest value
        state.apply_inst_test(&Sltu::new(A0, S0, S1));
        assert_eq!(state.regfile_read(A0), DataLword::zero());
    }

    #[test]
    fn test_shifts() {
        use RiscVRegister::*;
        let mut state = get_init_state();
        state.regfile_set(S1, DataLword::from(0xDEAD_BEEF_u32));
        state.regfile_set(S2, DataLword::from(4));
        state.apply_inst_test(&Sra::new(A0, S1, S2));
        assert_eq!(state.regfile_read(A0), DataLword::from(0xFDEA_DBEE_u32));
        state.apply_inst_test(&Srl::new(A0, S1, S2));
        assert_eq!(state.regfile_read(A0), DataLword::from(0x0DEA_DBEE_u32));
    }

    /// Tests an R type instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_r_type<T: RType<W32b>>(
        state: &mut ProgramState<RiscV<W32b>, W32b>,
        args: Vec<RTestData>,
    ) {
        for RTestData { rs2, result } in args {
            state.apply_inst_test(&T::new(RD, RS1, rs2));
            assert_eq!(i32::from(state.regfile_read(RD)), result);
        }
    }

    /// The RISCV spec defines all arithmetic to be wrapping.
    #[test]
    fn test_add_overflow() {
        let mut state = get_init_state();
        let rs1_val = 11_0729_6010_i32;
        let rs2_val = 12_4243_4058_i32;
        state.regfile_set(RS1, DataLword::from(rs1_val));
        state.regfile_set(RS2_POS, DataLword::from(rs2_val));
        test_r_type::<Add>(
            &mut state,
            vec![RTestData {
                rs2: RS2_POS,
                result: rs1_val.wrapping_add(rs2_val),
            }],
        )
    }

    #[test]
    fn test_r_type_insts() {
        let mut state = get_init_state();
        test_r_type::<Add>(
            &mut state,
            vec![
                RTestData {
                    rs2: RS2_POS,
                    result: RS1_VAL + RS2_VAL_POS,
                },
                RTestData {
                    rs2: RS2_NEG,
                    result: RS1_VAL + RS2_VAL_NEG,
                },
            ],
        );
        test_r_type::<Sub>(
            &mut state,
            vec![
                RTestData {
                    rs2: RS2_POS,
                    result: RS1_VAL - RS2_VAL_POS,
                },
                RTestData {
                    rs2: RS2_NEG,
                    result: RS1_VAL - RS2_VAL_NEG,
                },
            ],
        );
        test_r_type::<And>(
            &mut state,
            vec![
                RTestData {
                    rs2: RS2_POS,
                    result: RS1_VAL & RS2_VAL_POS,
                },
                RTestData {
                    rs2: RS2_NEG,
                    result: RS1_VAL & RS2_VAL_NEG,
                },
            ],
        );
        test_r_type::<Or>(
            &mut state,
            vec![
                RTestData {
                    rs2: RS2_POS,
                    result: RS1_VAL | RS2_VAL_POS,
                },
                RTestData {
                    rs2: RS2_NEG,
                    result: RS1_VAL | RS2_VAL_NEG,
                },
            ],
        );
        test_r_type::<Xor>(
            &mut state,
            vec![
                RTestData {
                    rs2: RS2_POS,
                    result: RS1_VAL ^ RS2_VAL_POS,
                },
                RTestData {
                    rs2: RS2_NEG,
                    result: RS1_VAL ^ RS2_VAL_NEG,
                },
            ],
        );
    }
}

#[cfg(test)]
mod tests_64 {
    use super::{super::tests_64::get_init_state, *};
    use crate::architectures::riscv::*;
    use RiscVRegister::*;

    /// Tests addition.
    #[test]
    fn test_addw_add() {
        let mut state = get_init_state();
        let a_big: u64 = 0x1234_5678_9ABC_DEF0u64;
        let b_big: u64 = 0x1111;
        let a_small: u32 = 0xDEAD_BEEFu32;
        let b_small: u32 = 0xFFFF_1234u32;
        state.regfile_set(T1, a_big.into());
        state.regfile_set(T2, b_big.into());
        state.regfile_set(T3, (a_small as u64).into());
        state.regfile_set(T4, (b_small as u64).into());
        // addw will truncate and sign extend
        state.apply_inst_test(&Addw::new(A0, T1, T2));
        assert_eq!(
            state.regfile_read(A0),
            (((a_big as i32) + (b_big as i32)) as u64).into()
        );
        state.apply_inst_test(&Addw::new(A0, T3, T4));
        assert_eq!(
            state.regfile_read(A0),
            (((a_small as i32) + (b_small as i32)) as i64).into()
        );
        // add does not truncate
        state.apply_inst_test(&Add::new(A0, T1, T2));
        assert_eq!(state.regfile_read(A0), (a_big + b_big).into());
        state.apply_inst_test(&Add::new(A0, T3, T4));
        assert_eq!(
            state.regfile_read(A0),
            ((a_small as i64) + (b_small as i64)).into()
        );
    }

    /// Tests subtraction.
    #[test]
    fn test_subw_sub() {
        let mut state = get_init_state();
        let a_big: u64 = 0x1234_5678_9ABC_DEF0u64;
        let b_big: u64 = 0x1111;
        let a_small: u32 = 0xDEAD_BEEFu32;
        let b_small: u32 = 0xFFFF_1234u32;
        state.regfile_set(T1, a_big.into());
        state.regfile_set(T2, b_big.into());
        state.regfile_set(T3, (a_small as u64).into());
        state.regfile_set(T4, (b_small as u64).into());
        // subw will truncate and sign extend
        state.apply_inst_test(&Subw::new(A0, T1, T2));
        assert_eq!(
            state.regfile_read(A0),
            (((a_big as i32) - (b_big as i32)) as u64).into()
        );
        state.apply_inst_test(&Subw::new(A0, T3, T4));
        assert_eq!(
            state.regfile_read(A0),
            (((a_small as i32) - (b_small as i32)) as i64).into()
        );
        // sub does not truncate
        state.apply_inst_test(&Sub::new(A0, T1, T2));
        assert_eq!(state.regfile_read(A0), (a_big - b_big).into());
        state.apply_inst_test(&Sub::new(A0, T3, T4));
        assert_eq!(
            state.regfile_read(A0),
            ((a_small as i64) - (b_small as i64)).into()
        );
    }
}
