//! Contains definitions for RISCV pseudo-instructions.
//! These definitions are reexported by the isa module.
//!
//! Immediates produced from labels are the value calculated by the assembler/linker.

use super::*;
use crate::{
    architectures::riscv::{instruction::*, registers::RiscVRegister},
    data_structures::*,
};
use RiscVRegister::*;

pub struct La;
impl La {
    pub fn expand_upper<S: AtLeast32b>(reg: RiscVRegister, data: RegValue<S>) -> RiscVInst<S> {
        let imm: BitStr32 = data.to_bit_str(32);
        let mut upper = imm.slice(31, 12);
        // Offset sign extension if needed
        // Do NOT add 4 to lower - lower adds 4 to its immediate because the immediate it sees
        // is already decremented by 4 since it's provided 1 instruction later
        if imm.index(11).as_u32() > 0 {
            upper = BitStr32::new(upper.as_u32() + 1, 20);
        }
        Auipc::new(reg, UnsignedValue::<S>::from(upper).into())
    }

    pub fn expand_lower<S: AtLeast32b>(reg: RiscVRegister, data: RegValue<S>) -> RiscVInst<S> {
        let imm: BitStr32 = data.to_bit_str(32);
        // Because auipc was one instruction before us and data is the offset from ourselves,
        // add 4 to the difference to account for the extra instruction
        let lower = BitStr32::new(imm.slice(11, 0).as_u32() + 4, 12);
        Addi::new(reg, reg, SignedValue::<S>::from(lower).into())
    }
}

pub struct Li32;
impl Li32 {
    pub fn expand(reg: RiscVRegister, data: DataLword) -> Vec<RiscVInst<W32b>> {
        let imm = data.to_bit_str(32);
        let mut upper = imm.slice(32, 12);
        let lower = imm.slice(11, 0);
        // If upper is not all 1s (which means the number isn't just a negative addi) and the MSB
        // of lower is high, then we need to add 1 to the upper immediate for sign extension reasons
        // The former case (all 1s) is covered by adding 1 due to overflow being truncated
        if lower.index(11).as_u32() > 0 {
            // This will never overflow because upper only has 20 bits of data
            upper = BitStr32::new(upper.as_u32() + 1, 20);
        }
        let no_lui = upper.is_zero();
        let rs1 = if no_lui { Zero } else { reg };
        let addi = Addi::new(reg, rs1, SignedValue::<W32b>::from(lower).into());
        if no_lui {
            vec![addi]
        } else {
            vec![
                Lui::new(reg, UnsignedValue::<W32b>::from(upper).into()),
                addi,
            ]
        }
    }
}

// For 64-bit literals, the literal is stored at a known location
// and li is emmitted as lui + ld
// TODO figure out how to emit a new label for 64-bit literals that we can ld from
pub struct Li64;
impl Li64 {
    pub fn expand(reg: RiscVRegister, data: DataDword) -> Vec<RiscVInst<W64b>> {
        let imm = data.to_bit_str(32);
        let mut upper = imm.slice(32, 12);
        let lower = imm.slice(11, 0);
        // If upper is not all 1s (which means the number isn't just a negative addi) and the MSB
        // of lower is high, then we need to add 1 to the upper immediate for sign extension reasons
        // The former case (all 1s) is covered by adding 1 due to overflow being truncated
        if lower.index(11).as_u32() > 0 {
            // This will never overflow because upper only has 20 bits of data
            upper = BitStr32::new(upper.as_u32() + 1, 20);
        }
        let no_lui = upper.is_zero();
        let rs1 = if no_lui { Zero } else { reg };
        let addi = Addi::new(reg, rs1, SignedValue::<W64b>::from(lower).into());
        if upper.is_zero() {
            vec![addi]
        } else {
            vec![
                Lui::new(reg, UnsignedValue::<W64b>::from(upper).into()),
                addi,
            ]
        }
    }
}

pub struct Mv;
impl Mv {
    pub fn expand<S: AtLeast32b>(rd: RiscVRegister, rs: RiscVRegister) -> RiscVInst<S> {
        Addi::new(rd, rs, <RegValue<S>>::zero())
    }
}

pub struct Neg;
impl Neg {
    pub fn expand<S: AtLeast32b>(rd: RiscVRegister, rs: RiscVRegister) -> RiscVInst<S> {
        Sub::new(rd, RiscVRegister::Zero, rs)
    }
}

pub struct Nop;
impl Nop {
    pub fn expand<S: AtLeast32b>() -> RiscVInst<S> {
        Addi::new(Zero, Zero, <RegValue<S>>::zero())
    }
}

pub struct Not;
impl Not {
    pub fn expand<S: AtLeast32b>(rd: RiscVRegister, rs: RiscVRegister) -> RiscVInst<S> {
        Xori::new(rd, rs, (-1i64).into())
    }
}

pub struct JalPseudo;
impl JalPseudo {
    pub fn expand<S: AtLeast32b>(offs: RegValue<S>) -> RiscVInst<S> {
        Jal::new(Ra, offs)
    }
}

pub struct JalrPseudo;
impl JalrPseudo {
    pub fn expand<S: AtLeast32b>(rs: RiscVRegister) -> RiscVInst<S> {
        Jalr::new(Ra, rs, <RegValue<S>>::zero())
    }
}

pub struct J;
impl J {
    pub fn expand<S: AtLeast32b>(offs: RegValue<S>) -> RiscVInst<S> {
        Jal::new(Zero, offs)
    }
}

pub struct Jr;
impl Jr {
    pub fn expand<S: AtLeast32b>(rs: RiscVRegister) -> RiscVInst<S> {
        Jalr::new(Zero, rs, <RegValue<S>>::zero())
    }
}

pub struct Ret;
impl Ret {
    pub fn expand<S: AtLeast32b>() -> RiscVInst<S> {
        Jalr::new(Zero, Ra, <RegValue<S>>::zero())
    }
}

pub struct SextW;
impl SextW {
    pub fn expand(rd: RiscVRegister, rs: RiscVRegister) -> RiscVInst<W64b> {
        Addiw::new(rd, rs, DataDword::zero())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        architectures::riscv::RiscV,
        data_structures::{DataLword, W32b},
        program_state::ProgramState,
    };

    type Expanded = Vec<RiscVInst<W32b>>;

    #[test]
    fn test_li_32() {
        // Tests various expansions of li
        // First one requires messing with sign
        let dead_beef: Expanded = Li32::expand(A0, DataLword::from(0xDEAD_BEEFu32));
        assert_eq!(dead_beef.len(), 2);
        assert_eq!(dead_beef[0], Lui::new(A0, DataLword::from(0xD_EADC)));
        assert_eq!(dead_beef[1], Addi::new(A0, A0, DataLword::from(-273)));
        // Edge case for sign
        let eef: Expanded = Li32::expand(A0, DataLword::from(0xEEF));
        assert_eq!(eef.len(), 2);
        assert_eq!(eef[0], Lui::new(A0, DataLword::from(1)));
        assert_eq!(eef[1], Addi::new(A0, A0, DataLword::from(-273)));
        // Negative, but lower is just -1
        let low_neg_1: Expanded = Li32::expand(A0, DataLword::from(0xFFAB_FFFFu32));
        assert_eq!(low_neg_1.len(), 2);
        assert_eq!(low_neg_1[0], Lui::new(A0, DataLword::from(0xF_FAC0)));
        assert_eq!(low_neg_1[1], Addi::new(A0, A0, DataLword::from(-1)));
        // Another such case with more complicated lower
        let abcd_abcd: Expanded = Li32::expand(A0, DataLword::from(0xABCD_ABCDu32));
        assert_eq!(abcd_abcd.len(), 2);
        assert_eq!(abcd_abcd[0], Lui::new(A0, DataLword::from(0xABCDB)));
        assert_eq!(abcd_abcd[1], Addi::new(A0, A0, DataLword::from(-1075)));
        // Negative, but just expands to addi
        let num: Expanded = Li32::expand(A0, DataLword::from(-273));
        assert_eq!(num.len(), 1);
        assert_eq!(num[0], Addi::new(A0, Zero, DataLword::from(-273)));
    }

    #[test]
    fn test_not_neg() {
        let mut state: ProgramState<RiscV<W64b>, W64b> = Default::default();
        let v1 = 0xABCD_ABCD_0123_0123u64 as i64;
        state.regfile_set(S0, v1.into());
        state.apply_inst_test(&Not::expand(A0, S0));
        assert_eq!(state.regfile_read(A0), (!v1).into());
        state.apply_inst_test(&Not::expand(A0, A0));
        assert_eq!(state.regfile_read(A0), v1.into());
        state.apply_inst_test(&Neg::expand(A0, S0));
        assert_eq!(state.regfile_read(A0), (-v1).into());
        state.apply_inst_test(&Neg::expand(A0, A0));
        assert_eq!(state.regfile_read(A0), v1.into());
    }

    /// This expansion is generated from the following code snippet, and cross-checked with Venus
    ///
    /// addi a0, zero, 5
    /// la t0, v
    /// lw a1, 0(t0)
    /// sw a0, 0(t0)
    /// .data
    /// v: .word 4
    #[test]
    fn test_la_auipc_expansion() {
        let offs = DataLword::from(0x1000_0000i32 - 4);
        let offs_p4 = DataLword::from(0x1000_0000i32 - 8);
        assert_eq!(La::expand_upper(A0, offs), Auipc::new(A0, 65536i32.into()));
        assert_eq!(
            La::expand_lower(A0, offs_p4),
            Addi::new(A0, A0, (-4i32).into())
        );
    }
}
