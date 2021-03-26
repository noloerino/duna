//! Instructions from the Zicsr extension.
//!
//! All these instructions follow I-type encodings from the base ISA.
use super::f3;
use crate::{
    architectures::riscv::{instruction::*, RiscV, RiscVRegister},
    data_structures::*,
    program_state::*,
};

const SYS_OPCODE: BitStr32 = BitStr32::new(0b111_0011, 7);

/// Checks the validity of the CSR.
fn csr_valid_check(_csrno: usize) -> Result<(), TermCause> {
    todo!()
}

/// Atomic Read/Write CSR
/// Copies the values in the CSR to rd, and from rs1 to the CSR. If rd is x0, then the CSR is not
/// read and any side effects from CSR reads do not occur.
pub struct Csrrw;
impl<S: AtLeast32b> IType<S> for Csrrw {
    fn name() -> &'static str {
        "csrrw"
    }

    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b001),
            opcode: SYS_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let csrno = imm.as_usize();
        csr_valid_check(csrno)?;
        Ok(if rd == RiscVRegister::Zero {
            vec![
                // Do not read CSR if rd is x0
                PrivDiff::csr_write(
                    &state.priv_state,
                    imm.as_usize(),
                    UnsignedValue::<S>::zero().into(),
                )
                .into_state_diff(),
                UserDiff::pc_p4(&state.user_state).into_state_diff(),
            ]
        } else {
            let priv_state = &state.priv_state;
            let user_state = &state.user_state;
            // TODO make this swap atomic?
            vec![
                // Read old value of CSR and write it to RD (our implementation doesn't need
                // to zero-extend since sizes match)
                UserDiff::reg_update(user_state, rd, priv_state.csr_read(csrno)).into_state_diff(),
                // Read value of RS1 and store it into CSR
                PrivDiff::csr_write(priv_state, csrno, user_state.regfile.read(rs1))
                    .into_state_diff(),
                UserDiff::pc_p4(user_state).into_state_diff(),
            ]
        })
    }
}

/// Atomic Read and Set Bits in CSR
/// Reads the CSR into rd; any bit high in rs1 will be set high if possible in the CSR.
pub struct Csrrs;
impl<S: AtLeast32b> IType<S> for Csrrs {
    fn name() -> &'static str {
        "csrrs"
    }

    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b010),
            opcode: SYS_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let csrno = imm.as_usize();
        csr_valid_check(csrno)?;
        let priv_state = &state.priv_state;
        let user_state = &state.user_state;
        Ok(if rs1 == RiscVRegister::Zero {
            vec![
                // Read old value of CSR and write it to RD (our implementation doesn't need
                // to zero-extend since sizes match)
                UserDiff::reg_update(user_state, rd, priv_state.csr_read(csrno)).into_state_diff(),
                // Do not write CSR if rs1 is x0
                UserDiff::pc_p4(user_state).into_state_diff(),
            ]
        } else {
            let csrval = priv_state.csr_read(csrno);
            vec![
                // Read old value of CSR and write it to RD (our implementation doesn't need
                // to zero-extend since sizes match)
                UserDiff::reg_update(user_state, rd, csrval).into_state_diff(),
                // Set bits in CSR high
                PrivDiff::csr_write(priv_state, csrno, csrval | user_state.regfile.read(rs1))
                    .into_state_diff(),
                UserDiff::pc_p4(user_state).into_state_diff(),
            ]
        })
    }
}

/// Atomic Read and Clear Bits in CSR
/// Reads the CSR into rd; any bit high in rs1 will be set to zero if possible in the CSR.
pub struct Csrrc;
impl<S: AtLeast32b> IType<S> for Csrrc {
    fn name() -> &'static str {
        "csrrs"
    }

    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b011),
            opcode: SYS_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let csrno = imm.as_usize();
        csr_valid_check(csrno)?;
        let priv_state = &state.priv_state;
        let user_state = &state.user_state;
        Ok(if rs1 == RiscVRegister::Zero {
            vec![
                // Read old value of CSR and write it to RD (our implementation doesn't need
                // to zero-extend since sizes match)
                UserDiff::reg_update(user_state, rd, priv_state.csr_read(csrno)).into_state_diff(),
                // Do not write CSR if rs1 is x0
                UserDiff::pc_p4(user_state).into_state_diff(),
            ]
        } else {
            let csrval = priv_state.csr_read(csrno);
            vec![
                // Read old value of CSR and write it to RD (our implementation doesn't need
                // to zero-extend since sizes match)
                UserDiff::reg_update(user_state, rd, csrval).into_state_diff(),
                // Clear bits in CSR
                PrivDiff::csr_write(priv_state, csrno, csrval & !user_state.regfile.read(rs1))
                    .into_state_diff(),
                UserDiff::pc_p4(user_state).into_state_diff(),
            ]
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use RiscVRegister::*;

    fn get_init_state() -> ProgramState<RiscV<W32b>, W32b> {
        ProgramState::<RiscV<W32b>, W32b>::default()
    }

    #[test]
    fn test_csr_zero() {
        let mut state = get_init_state();
        // 0xFF is a standard user-space R/W CSR
        let csrno: u64 = 0xFF;
        let csr_val = 0xABCD;
        state.csr_write(csrno as usize, csr_val.into());
        state.regfile_set(A0, 0x1234.into());
        state.regfile_set(S0, 0x5678.into());
        // When 0 is the mask, no bits in the CSR get changed
        state.apply_inst_test(&Csrrs::new(T0, Zero, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), csr_val.into());
        assert_eq!(state.regfile_read(T0), csr_val.into());
        state.apply_inst_test(&Csrrc::new(T0, Zero, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), csr_val.into());
        assert_eq!(state.regfile_read(T0), csr_val.into());
        state.apply_inst_test(&Csrrw::new(Zero, S0, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), 0.into());
    }

    #[test]
    fn test_csrrw() {
        let mut state = get_init_state();
        let csrno: u64 = 0xFF;
        let csr_val = 0xABCD;
        state.csr_write(csrno as usize, csr_val.into());
        state.regfile_set(A0, 0x1234.into());
        state.regfile_set(S0, 0x5678.into());
        state.apply_inst_test(&Csrrw::new(T0, A0, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), 0x1234.into());
        assert_eq!(state.regfile_read(T0), csr_val.into());
    }

    #[test]
    fn test_csrrs() {
        let mut state = get_init_state();
        let csrno: u64 = 0xFF;
        state.csr_write(csrno as usize, 0x1100.into());
        state.regfile_set(A0, 0x11.into());
        state.apply_inst_test(&Csrrs::new(T0, A0, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), 0x1111.into());
        assert_eq!(state.regfile_read(T0), 0x1100.into());
    }

    #[test]
    fn test_csrrc() {
        let mut state = get_init_state();
        let csrno: u64 = 0xFF;
        state.csr_write(csrno as usize, 0x1111.into());
        state.regfile_set(A0, 0x11.into());
        state.apply_inst_test(&Csrrc::new(T0, A0, csrno.into()));
        assert_eq!(state.csr_read(csrno as usize), 0x1100.into());
        assert_eq!(state.regfile_read(T0), 0x1111.into());
    }
}

// pub struct Csrrwi;
// pub struct Csrrsi;
// pub struct Csrrci;
