use crate::{
    architectures::mips::{exception::Exception, instruction::*, isa::*},
    data_structures::*,
};
use num_traits::ops::checked::CheckedAdd;

const OPCODE_ZERO: BitStr32 = BitStr32::new(0, 6);
const SHAMT_ZERO: BitStr32 = BitStr32::new(0, 5);

pub struct Add;

impl<S: AtLeast32b> RType<S> for Add {
    fn name() -> &'static str {
        "add"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            opcode: OPCODE_ZERO,
            shamt: SHAMT_ZERO,
            funct: funct(0x20),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> Result<RegValue<S>, Exception> {
        if let Some(r) = rs1_val.as_signed().checked_add(&rs2_val.as_signed()) {
            Ok(r.as_reg_data())
        } else {
            // MIPS ISA spec:
            // "If the addition results in 32-bit 2’s complement arithmetic overflow, the
            // destination register is not modified and an Integer Overflow exception occurs."
            Err(Exception::Overflow)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        architectures::mips::{
            arch::Mips,
            program::MipsCsr,
            registers::{MipsRegister, MipsRegister::*},
        },
        program_state::*,
    };

    pub fn get_init_state() -> ProgramState<Mips<W32b>, W32b> {
        let state: ProgramState<Mips<W32b>, W32b> = Default::default();
        state
    }

    /// Checks that an exception is raised on addition
    /// TODO check no exception for addu
    #[test]
    fn test_add_overflow() {
        let mut state = get_init_state();
        let rd_orig = state.regfile_read(MipsRegister::Fp);
        state.regfile_set(S0, 0x7FFF_FFFFu32.into());
        state.regfile_set(S1, 0x3.into());
        state.apply_inst(&Add::new(MipsRegister::Fp, S0, S1)).ok();
        // Ensure A0 hasn't changed
        assert_eq!(state.regfile_read(MipsRegister::Fp), rd_orig);
        // Check bits 2-5 of CSR
        assert_eq!(
            (u32::from(state.csr_read(MipsCsr::Cause as usize)) >> 2) & 0b1111,
            12
        );
    }

    #[test]
    fn test_add_no_overflow() {
        let mut state = get_init_state();
        state.regfile_set(S0, 0x1.into());
        state.regfile_set(S1, 0x1.into());
        state.apply_inst_test(&Add::new(A0, S0, S1));
        assert_eq!(state.regfile_read(A0), 0x2.into());
    }
}
