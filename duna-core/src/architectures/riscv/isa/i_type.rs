use super::super::instruction::*;
use super::super::*;
use super::f3;
use crate::program_state::*;
use duna_macro::*;
use num_traits::ops::wrapping::WrappingAdd;

const I_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_0011, 7);
const I_W_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_1011, 7);
const I_OPCODE_LOAD: BitStr32 = BitStr32::new(0b000_0011, 7);

#[derive(ITypeArith)]
pub struct Addi;
impl<S: AtLeast32b> ITypeArith<S> for Addi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: SignedValue<S> = imm.into();
        (v1.wrapping_add(&imm_val)).into()
    }
}

#[derive(ITypeArith64)]
pub struct Addiw;
impl ITypeArith<W64b> for Addiw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0),
            opcode: I_W_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: DataDword, imm: BitStr32) -> DataDword {
        let v1: DataLword = rs1_val.lower_lword();
        let v2: DataLword = SignedValue::<W32b>::from(imm).into();
        let result: DataLword = u32::from(v1).wrapping_add(u32::from(v2)).into();
        DataDword::sign_ext_from_lword(result)
    }
}

#[derive(ITypeArith)]
pub struct Andi;
impl<S: AtLeast32b> ITypeArith<S> for Andi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b111),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: SignedValue<S> = imm.into();
        (v1 & imm_val).into()
    }
}

pub struct Jalr;
impl<S: AtLeast32b> IType<S> for Jalr {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: BitStr32::new(0b110_0111, 7),
            funct3: f3(0b000),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let v1: SignedValue<S> = state.user_state.regfile.read(rs1).into();
        Ok(UserDiff::reg_write_op(
            &state.user_state,
            (v1.wrapping_add(&imm.into())).into(),
            rd,
            state.user_state.pc.plus_4().into(),
        ))
    }
}

#[derive(ITypeLoad)]
pub struct Lb;
impl<S: AtLeast32b> ITypeLoad<S> for Lb {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b000),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>> {
        let (v, diffs) = state.memory_get::<W8b>(addr)?;
        Ok((<RegValue<S>>::sign_ext_from_byte(v), diffs))
    }
}

#[derive(ITypeLoad)]
pub struct Lbu;
impl<S: AtLeast32b> ITypeLoad<S> for Lbu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b100),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>> {
        let (v, diffs) = state.memory_get::<W8b>(addr)?;
        Ok((<RegValue<S>>::zero_pad_from_byte(v), diffs))
    }
}

#[derive(ITypeLoad64)]
pub struct Ld;
impl ITypeLoad<W64b> for Ld {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b011),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<W64b>, W64b>,
        addr: ByteAddr64,
    ) -> Result<MemReadResult<W64b>, MemFault<W64b>> {
        let (v, diffs) = state.memory_get::<W64b>(addr)?;
        Ok((v, diffs))
    }
}

#[derive(ITypeLoad)]
pub struct Lh;
impl<S: AtLeast32b> ITypeLoad<S> for Lh {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b001),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>> {
        let (v, diffs) = state.memory_get::<W16b>(addr)?;
        Ok((<RegValue<S>>::sign_ext_from_half(v), diffs))
    }
}

#[derive(ITypeLoad)]
pub struct Lhu;
impl<S: AtLeast32b> ITypeLoad<S> for Lhu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b101),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>> {
        let (v, diffs) = state.memory_get::<W16b>(addr)?;
        Ok((<RegValue<S>>::zero_pad_from_half(v), diffs))
    }
}

#[derive(ITypeLoad)]
pub struct Lw;
impl<S: AtLeast32b> ITypeLoad<S> for Lw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b010),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        addr: ByteAddrValue<S>,
    ) -> Result<MemReadResult<S>, MemFault<S>> {
        let (v, diffs) = state.memory_get::<W32b>(addr)?;
        Ok((<RegValue<S>>::sign_ext_from_lword(v), diffs))
    }
}

#[derive(ITypeLoad64)]
pub struct Lwu;
impl ITypeLoad<W64b> for Lwu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b110),
        }
    }

    fn eval(
        state: &ProgramState<RiscV<W64b>, W64b>,
        addr: ByteAddr64,
    ) -> Result<MemReadResult<W64b>, MemFault<W64b>> {
        let (v, diffs) = state.memory_get::<W32b>(addr)?;
        Ok((DataDword::sign_ext_from_lword(v), diffs))
    }
}

#[derive(ITypeArith)]
pub struct Ori;
impl<S: AtLeast32b> ITypeArith<S> for Ori {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b110),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: SignedValue<S> = imm.into();
        (v1 | imm_val).into()
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

pub struct Slli;
impl<S: AtLeast32b> ITypeShift<S> for Slli {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b001),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0, 7)
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: UnsignedValue<S> = imm.into();
        (v1 << shamt(imm_val.as_reg_data()).as_signed()).into()
    }
}

pub struct Slliw;
impl ITypeShift<W64b> for Slliw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b001),
            opcode: I_W_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0, 7)
    }

    fn eval(rs1_val: RegValue<W64b>, imm: BitStr32) -> RegValue<W64b> {
        let v1 = rs1_val.lower_lword().as_unsigned();
        let imm_val: UnsignedValue<W32b> = imm.into();
        DataDword::sign_ext_from_lword(v1.as_reg_data() << shamt(imm_val.as_reg_data()))
            .as_reg_data()
    }
}

pub struct Srai;
impl<S: AtLeast32b> ITypeShift<S> for Srai {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b101),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0b010_0000, 7)
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: UnsignedValue<S> = imm.into();
        (v1 >> shamt(imm_val.as_reg_data()).as_signed()).into()
    }
}

pub struct Sraiw;
impl ITypeShift<W64b> for Sraiw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b101),
            opcode: I_W_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0b010_0000, 7)
    }

    fn eval(rs1_val: RegValue<W64b>, imm: BitStr32) -> RegValue<W64b> {
        let v1 = rs1_val.lower_lword().as_signed();
        let imm_val: UnsignedValue<W32b> = imm.into();
        DataDword::sign_ext_from_lword(
            (v1 >> shamt(imm_val.as_reg_data()).as_signed()).as_reg_data(),
        )
    }
}

pub struct Srli;
impl<S: AtLeast32b> ITypeShift<S> for Srli {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b101),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0, 7)
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: UnsignedValue<S> = rs1_val.into();
        let imm_val: UnsignedValue<S> = imm.into();
        (v1 >> shamt(imm_val.as_reg_data()).as_unsigned()).into()
    }
}

pub struct Srliw;
impl ITypeShift<W64b> for Srliw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b101),
            opcode: I_W_OPCODE_ARITH,
        }
    }

    fn f7() -> BitStr32 {
        BitStr32::new(0, 7)
    }

    fn eval(rs1_val: RegValue<W64b>, imm: BitStr32) -> RegValue<W64b> {
        let v1 = rs1_val.lower_lword().as_unsigned();
        let imm_val: UnsignedValue<W32b> = imm.into();
        DataDword::sign_ext_from_lword(
            (v1 >> shamt(imm_val.as_reg_data()).as_unsigned()).as_reg_data(),
        )
    }
}

#[derive(ITypeArith)]
pub struct Xori;
impl<S: AtLeast32b> ITypeArith<S> for Xori {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b100),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: RegValue<S>, imm: BitStr32) -> RegValue<S> {
        let v1: SignedValue<S> = rs1_val.into();
        let imm_val: SignedValue<S> = imm.into();
        (v1 ^ imm_val).into()
    }
}

#[cfg(test)]
mod tests_32 {
    use super::super::tests_32::*;
    use super::*;
    use RiscVRegister::*;

    struct IArithTestData {
        imm: i32,
        result: i32,
    }

    /// Tests an I type arithmetic instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_i_type_arith<T: ITypeArith<W32b>>(
        state: &mut ProgramState<RiscV<W32b>, W32b>,
        args: Vec<IArithTestData>,
    ) {
        for IArithTestData { imm, result } in args {
            state.apply_inst_test(&T::new(RD, RS1, DataLword::from(imm)));
            println!(
                "rd actual:\t{:b}\nrd expected:\t{:b}",
                i32::from(state.regfile_read(RD)),
                result
            );
            assert_eq!(i32::from(state.regfile_read(RD)), result);
        }
    }

    fn test_i_type_shift<T: ITypeShift<W32b>>(
        state: &mut ProgramState<RiscV<W32b>, W32b>,
        args: Vec<IArithTestData>,
    ) {
        for IArithTestData { imm, result } in args {
            state.apply_inst_test(&T::new(RD, RS1, DataLword::from(imm)));
            println!(
                "rd actual:\t{:b}\nrd expected:\t{:b}",
                i32::from(state.regfile_read(RD)),
                result
            );
            assert_eq!(i32::from(state.regfile_read(RD)), result);
        }
    }

    #[test]
    fn test_i_type_arith_insts() {
        let mut state = get_init_state();
        println!("testing ADDI");
        test_i_type_arith::<Addi>(
            &mut state,
            vec![
                IArithTestData {
                    imm: -504,
                    result: RS1_VAL - 504,
                },
                IArithTestData {
                    imm: -1,
                    result: RS1_VAL - 1,
                },
                IArithTestData {
                    imm: 1024,
                    result: RS1_VAL + 1024,
                },
                IArithTestData {
                    imm: -1075,
                    result: RS1_VAL - 1075,
                },
            ],
        );
        println!("testing ANDI");
        test_i_type_arith::<Andi>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0b1100,
                    result: RS1_VAL & 0b1100,
                },
                IArithTestData {
                    imm: 0xABCu32 as i32,
                    // account for sign extension of 12-bit immediate
                    result: RS1_VAL & 0xFFFF_FABCu32 as i32,
                },
            ],
        );
        println!("testing ORI");
        test_i_type_arith::<Ori>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0b1100,
                    result: RS1_VAL | 0b1100,
                },
                IArithTestData {
                    imm: 0xABCu32 as i32,
                    result: RS1_VAL | 0xFFFF_FABCu32 as i32,
                },
            ],
        );
        println!("testing XORI");
        test_i_type_arith::<Xori>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0b1100,
                    result: RS1_VAL ^ 0b1100,
                },
                IArithTestData {
                    imm: 0xABCu32 as i32,
                    result: RS1_VAL ^ 0xFFFF_FABCu32 as i32,
                },
            ],
        );
    }

    #[test]
    fn test_jalr() {
        let mut state = get_init_state();
        let starting_pc_val = 0x10FC_0000;
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let tgt_pc_val = 0xABCD_EF10u32;
        let inst = Jalr::new(RA, RS1, DataLword::from(-4));
        let exp_pc_val = tgt_pc_val - 4;
        state.regfile_set(RS1, DataLword::from(tgt_pc_val));
        state.apply_inst_test(&inst);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(exp_pc_val));
    }

    #[test]
    fn test_lb_lbu() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(ByteAddr32::from(base_addr), DataLword::from(test_data));
        // Make sure the issue isn't with poking the memory
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(base_addr)),
            DataLword::from(test_data)
        );
        state.regfile_set(T0, DataLword::from(base_addr));
        // signed loads
        state.apply_inst_test(&Lb::new(T1, T0, DataLword::from(0)));
        state.apply_inst_test(&Lb::new(T2, T0, DataLword::from(1)));
        state.apply_inst_test(&Lb::new(T3, T0, DataLword::from(2)));
        state.apply_inst_test(&Lb::new(T4, T0, DataLword::from(3)));
        assert_eq!(state.regfile_read(T1), DataLword::from(0x01));
        assert_eq!(state.regfile_read(T2), DataLword::from(0xFFFF_FFEFu32));
        assert_eq!(state.regfile_read(T3), DataLword::from(0xFFFF_FFCDu32));
        assert_eq!(state.regfile_read(T4), DataLword::from(0xFFFF_FFABu32));
        // unsigned loads
        state.apply_inst_test(&Lbu::new(T1, T0, DataLword::from(0)));
        state.apply_inst_test(&Lbu::new(T2, T0, DataLword::from(1)));
        state.apply_inst_test(&Lbu::new(T3, T0, DataLword::from(2)));
        state.apply_inst_test(&Lbu::new(T4, T0, DataLword::from(3)));
        assert_eq!(state.regfile_read(T1), DataLword::from(0x01));
        assert_eq!(state.regfile_read(T2), DataLword::from(0xEF));
        assert_eq!(state.regfile_read(T3), DataLword::from(0xCD));
        assert_eq!(state.regfile_read(T4), DataLword::from(0xAB));
    }

    #[test]
    fn test_lh_lhu_aligned() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0x0BCD_EF01u32;
        state.memory_set_word(ByteAddr32::from(base_addr), DataLword::from(test_data));
        state.regfile_set(T0, DataLword::from(base_addr));
        // signed loads
        state.apply_inst_test(&Lh::new(T1, T0, DataLword::from(0)));
        state.apply_inst_test(&Lh::new(T2, T0, DataLword::from(2)));
        assert_eq!(state.regfile_read(T1), DataLword::from(0xFFFF_EF01u32));
        assert_eq!(state.regfile_read(T2), DataLword::from(0x0BCD));
        // unsigned loads
        state.apply_inst_test(&Lhu::new(T1, T0, DataLword::from(0)));
        state.apply_inst_test(&Lhu::new(T2, T0, DataLword::from(2)));
        assert_eq!(state.regfile_read(T1), DataLword::from(0xEF01));
        assert_eq!(state.regfile_read(T2), DataLword::from(0x0BCD));
    }

    #[test]
    fn test_lw_aligned() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(ByteAddr32::from(base_addr), DataLword::from(test_data));
        state.regfile_set(T0, DataLword::from(base_addr));
        state.apply_inst_test(&Lw::new(T1, T0, DataLword::from(0)));
        assert_eq!(state.regfile_read(T1), DataLword::from(test_data));
        // Test loading with negative offset
        state.regfile_set(T0, DataLword::from(base_addr + 16));
        state.apply_inst_test(&Lw::new(T1, T0, DataLword::from(-16)));
        assert_eq!(state.regfile_read(T1), DataLword::from(test_data));
    }

    #[test]
    fn test_shifts() {
        let mut state = get_init_state();
        test_i_type_shift::<Slli>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0,
                    result: RS1_VAL,
                },
                IArithTestData {
                    imm: 2,
                    result: RS1_VAL << 2,
                },
            ],
        );
        test_i_type_shift::<Srai>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0,
                    result: RS1_VAL,
                },
                IArithTestData {
                    imm: 2,
                    result: RS1_VAL as i32 >> 2,
                },
            ],
        );
        test_i_type_shift::<Srli>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0,
                    result: RS1_VAL,
                },
                IArithTestData {
                    imm: 2,
                    result: ((RS1_VAL as u32) >> 2) as i32,
                },
            ],
        );
        // Test signed behavior
        state.regfile_set(S1, DataLword::from(0xDEAD_BEEF_u32));
        state.apply_inst_test(&Srai::new(S2, S1, DataLword::from(4)));
        assert_eq!(state.regfile_read(S2), DataLword::from(0xFDEA_DBEE_u32));
        state.apply_inst_test(&Srli::new(S2, S1, DataLword::from(4)));
        assert_eq!(state.regfile_read(S2), DataLword::from(0x0DEA_DBEE_u32));
    }
}

#[cfg(test)]
mod tests_64 {
    use super::super::tests_64::get_init_state;
    use super::*;
    use RiscVRegister::*;

    #[test]
    fn test_ld_aligned() {
        let mut state = get_init_state();
        let val: DataDword = 0xABCD_ABCD_0123_0123u64.into();
        let addr: ByteAddr64 = 0x7FFF_FFFF_FFFF_FFF0u64.into();
        state.memory_set_doubleword(addr, val);
        state.regfile_set(S1, addr.into());
        state.apply_inst_test(&Ld::new(S2, S1, DataDword::zero()));
        assert_eq!(state.regfile_read(S2), val);
    }

    #[test]
    fn test_lwu_aligned() {
        let mut state = get_init_state();
        // this value will be sign extended
        let val: DataLword = 0xF123_0123u32.into();
        let addr: ByteAddr64 = 0x7FFF_FFFF_FFFF_FFF0u64.into();
        state.memory_set_word(addr, val);
        state.regfile_set(S1, addr.into());
        state.apply_inst_test(&Lwu::new(S2, S1, DataDword::zero()));
        assert_eq!(state.regfile_read(S2), DataDword::sign_ext_from_lword(val));
        // this value will not be sign extended
        let val2: DataLword = 0x0123_0123u32.into();
        state.memory_set_word(addr, val2);
        state.regfile_set(T1, addr.into());
        state.apply_inst_test(&Lwu::new(T2, T1, DataDword::zero()));
        assert_eq!(state.regfile_read(T2), DataDword::zero_pad_from_lword(val2));
    }

    #[test]
    fn test_w_shifts() {
        let mut state = get_init_state();
        // this value will be truncated to 32 bits, then sign extended
        let val: DataDword = 0xDEAD_BEEFu64.into();
        state.regfile_set(S0, val);
        state.apply_inst_test(&Slliw::new(T2, S0, DataDword::from(4u64)));
        assert_eq!(
            state.regfile_read(T2),
            DataDword::from(0xFFFF_FFFF_EADB_EEF0u64)
        );
        state.apply_inst_test(&Sraiw::new(T3, S0, DataDword::from(4u64)));
        assert_eq!(
            state.regfile_read(T3),
            DataDword::from(0xFFFF_FFFF_FDEA_DBEEu64)
        );
        state.apply_inst_test(&Srliw::new(T4, S0, DataDword::from(4u64)));
        assert_eq!(state.regfile_read(T4), DataDword::from(0x0DEA_DBEEu64));
    }

    #[test]
    fn test_6b_shift() {
        // Unlike RV32, RV64 supports shifts of up to 6 bits
        let mut state = get_init_state();
        let val: DataDword = 0x0123_4567_89AB_CDEFu64.into();
        state.regfile_set(S0, val);
        state.apply_inst_test(&Slli::new(S11, S0, DataDword::from(63u64)));
        assert_eq!(
            state.regfile_read(S11),
            DataDword::from(0x8000_0000_0000_0000u64)
        );
    }
}
