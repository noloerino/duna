//! Instructions from the base integer instruction set.
mod i_type;
mod pseudo;
mod r_type;

use crate::{
    architectures::riscv::{arch::*, instruction::*, registers::RiscVRegister},
    program_state::*,
};
pub use i_type::*;
use num_traits::ops::wrapping::WrappingAdd;
pub use pseudo::*;
pub use r_type::*;

pub(crate) fn f3(val: u32) -> BitStr32 {
    BitStr32::new(val, 3)
}

pub(crate) fn f7(val: u32) -> BitStr32 {
    BitStr32::new(val, 7)
}

const SYS_OPCODE: BitStr32 = BitStr32::new(0b111_0011, 7);
const B_OPCODE: BitStr32 = BitStr32::new(0b110_0011, 7);
const J_OPCODE: BitStr32 = BitStr32::new(0b110_1111, 7);
const S_OPCODE: BitStr32 = BitStr32::new(0b010_0011, 7);

pub struct Auipc;
impl<S: AtLeast32b> UType<S> for Auipc {
    fn name() -> &'static str {
        "auipc"
    }

    fn inst_fields() -> UInstFields {
        UInstFields {
            opcode: BitStr32::new(0b001_0111, 7),
        }
    }

    fn eval(
        state: &UserState<RiscV<S>, S>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> DiffStack<RiscV<S>, S> {
        let pc: SignedValue<S> = state.pc.into();
        UserDiff::reg_write_pc_p4(
            state,
            rd,
            (pc.wrapping_add(&imm.zero_pad_lsb().into())).into(),
        )
    }
}

pub struct Beq;
impl<S: AtLeast32b> BType<S> for Beq {
    fn name() -> &'static str {
        "beq"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val == rs2_val
    }
}

pub struct Bge;
impl<S: AtLeast32b> BType<S> for Bge {
    fn name() -> &'static str {
        "bge"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b101),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val.as_signed().raw() >= rs2_val.as_signed().raw()
    }
}

pub struct Bgeu;
impl<S: AtLeast32b> BType<S> for Bgeu {
    fn name() -> &'static str {
        "bgeu"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b111),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val.as_unsigned().raw() >= rs2_val.as_unsigned().raw()
    }
}

pub struct Blt;
impl<S: AtLeast32b> BType<S> for Blt {
    fn name() -> &'static str {
        "blt"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b100),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val.as_signed().raw() < rs2_val.as_signed().raw()
    }
}

pub struct Bltu;
impl<S: AtLeast32b> BType<S> for Bltu {
    fn name() -> &'static str {
        "bltu"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b110),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val.as_unsigned().raw() < rs2_val.as_unsigned().raw()
    }
}

pub struct Bne;
impl<S: AtLeast32b> BType<S> for Bne {
    fn name() -> &'static str {
        "bne"
    }

    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b001),
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> bool {
        rs1_val != rs2_val
    }
}

pub struct Ecall;
impl<S: AtLeast32b> EnvironInst<S> for Ecall {
    fn name() -> &'static str {
        "ecall"
    }

    fn funct12() -> BitStr32 {
        BitStr32::new(0, 12)
    }

    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: SYS_OPCODE,
            funct3: f3(0b000),
        }
    }

    fn eval(state: &ProgramState<RiscV<S>, S>) -> InstResult<RiscV<S>, S> {
        let mut diffs = state.handle_trap(&TrapKind::Ecall)?;
        diffs.push(UserDiff::pc_p4(&state.user_state).into_state_diff());
        Ok(diffs)
    }
}

pub struct Jal;
impl<S: AtLeast32b> JType<S> for Jal {
    fn name() -> &'static str {
        "jal"
    }

    fn inst_fields() -> JInstFields {
        JInstFields { opcode: J_OPCODE }
    }

    fn eval(
        state: &UserState<RiscV<S>, S>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> DiffStack<RiscV<S>, S> {
        let pc: SignedValue<S> = state.pc.into();
        let offs: SignedValue<S> = imm.into();
        UserDiff::reg_write_op(
            state,
            (pc.wrapping_add(&offs)).into(),
            rd,
            state.pc.plus_4().into(),
        )
    }
}

pub struct Lui;
impl<S: AtLeast32b> UType<S> for Lui {
    fn name() -> &'static str {
        "lui"
    }

    fn inst_fields() -> UInstFields {
        UInstFields {
            opcode: BitStr32::new(0b011_0111, 7),
        }
    }

    fn eval(
        state: &UserState<RiscV<S>, S>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> DiffStack<RiscV<S>, S> {
        let imm_val: SignedValue<S> = imm.zero_pad_lsb().into();
        UserDiff::reg_write_pc_p4(state, rd, imm_val.into())
    }
}

pub struct Sb;
impl<S: AtLeast32b> SType<S> for Sb {
    fn name() -> &'static str {
        "sb"
    }

    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b000),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let base_addr: SignedValue<S> = state.user_state.regfile.read(rs1).into();
        let byte_addr: ByteAddrValue<S> = (base_addr.wrapping_add(&imm.into())).into();
        let new_byte = state.user_state.regfile.read(rs2).get_byte(0);
        UserDiff::mem_write_pc_p4(state, byte_addr, DataEnum::Byte(new_byte))
            .map_err(TermCause::from)
    }
}

pub struct Sd;
impl SType<W64b> for Sd {
    fn name() -> &'static str {
        "sd"
    }

    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b011),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<W64b>, W64b>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<W64b>, W64b> {
        let base_addr: i64 = state.user_state.regfile.read(rs1).into();
        let byte_addr: ByteAddr64 = (base_addr.wrapping_add(imm.into())).into();
        let new_dword = state.user_state.regfile.read(rs2);
        UserDiff::mem_write_pc_p4(state, byte_addr, DataEnum::Dword(new_dword))
            .map_err(TermCause::from)
    }
}

pub struct Sh;
impl<S: AtLeast32b> SType<S> for Sh {
    fn name() -> &'static str {
        "sh"
    }

    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b001),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let base_addr: SignedValue<S> = state.user_state.regfile.read(rs1).into();
        let byte_addr: ByteAddrValue<S> = (base_addr.wrapping_add(&imm.into())).into();
        let lower_byte: u8 = state.user_state.regfile.read(rs2).get_byte(0).into();
        let upper_byte: u8 = state.user_state.regfile.read(rs2).get_byte(1).into();
        let full: u16 = ((upper_byte as u16) << 8) | (lower_byte as u16);
        UserDiff::mem_write_pc_p4(state, byte_addr, DataEnum::Half(full.into()))
            .map_err(TermCause::from)
    }
}

pub struct Sw;
impl<S: AtLeast32b> SType<S> for Sw {
    fn name() -> &'static str {
        "sw"
    }

    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b010),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &ProgramState<RiscV<S>, S>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<S>, S> {
        let base_addr: SignedValue<S> = state.user_state.regfile.read(rs1).into();
        let byte_addr: ByteAddrValue<S> = (base_addr.wrapping_add(&imm.into())).into();
        UserDiff::mem_write_pc_p4(
            state,
            byte_addr,
            DataEnum::Lword(state.user_state.regfile.read(rs2).lower_lword()),
        )
        .map_err(TermCause::from)
    }
}

/// Tests for the base 32-bit ISA.
#[cfg(test)]
mod tests_32 {
    use super::*;
    use crate::architectures::riscv::program::RiscVSyscallConvention;
    use crate::instruction::*;
    use crate::program_state::Syscall;
    use RiscVRegister::*;

    pub const RS1_VAL: i32 = 1023;
    pub const RS2_VAL_POS: i32 = 49;
    pub const RS2_VAL_NEG: i32 = -1;
    pub const RD: RiscVRegister = T2;
    pub const RS1: RiscVRegister = T0;
    pub const RS2: RiscVRegister = T3;
    pub const RS2_POS: RiscVRegister = T1;
    pub const RS2_NEG: RiscVRegister = S1;

    pub fn get_init_state() -> ProgramState<RiscV<W32b>, W32b> {
        let mut state: ProgramState<RiscV<W32b>, W32b> = Default::default();
        state.regfile_set(RS1, DataLword::from(RS1_VAL));
        state.regfile_set(RS2_POS, DataLword::from(RS2_VAL_POS));
        state.regfile_set(RS2_NEG, DataLword::from(RS2_VAL_NEG));
        state
    }

    #[test]
    fn test_write_x0() {
        let mut state: ProgramState<RiscV<W32b>, W32b> = Default::default();
        state.apply_inst_test(&Addi::new(ZERO, ZERO, DataLword::from(0x100)));
        assert_eq!(i32::from(state.regfile_read(ZERO)), 0);
    }

    #[test]
    /// Tests machine code conversions of instructions.
    /// These conversions were done by typing the instruction into Venus.
    fn test_to_machine_code() {
        // add s0, s1, s2
        const ADD_HEX: u32 = 0x0124_8433;
        let add_inst: RiscVInst<W32b> = Add::new(RiscVRegister::S0, S1, S2);
        assert_eq!(add_inst.to_machine_code(), ADD_HEX);
        // addi T1, T1, -1075
        const ADDI_HEX: u32 = 0xBCD3_0313;
        let addi_inst: RiscVInst<W32b> = Addi::new(T1, T1, DataLword::from(-1075));
        assert_eq!(addi_inst.to_machine_code(), ADDI_HEX);
        // auipc s1, 10
        const AUIPC_HEX: u32 = 0x0000_A497;
        let auipc_inst: RiscVInst<W32b> = Auipc::new(S1, DataLword::from(10));
        assert_eq!(auipc_inst.to_machine_code(), AUIPC_HEX);
        // bne s1, s2, 4
        const BNE_HEX: u32 = 0x0124_9263;
        let bne_inst: RiscVInst<W32b> = Bne::new(S1, S2, DataLword::from(4));
        assert_eq!(bne_inst.to_machine_code(), BNE_HEX);
        // ecall
        const ECALL_HEX: u32 = 0x0000_0073;
        let ecall_inst: RiscVInst<W32b> = Ecall::new();
        assert_eq!(ecall_inst.to_machine_code(), ECALL_HEX);
        // jal ra, 16
        const JAL_HEX: u32 = 0x0100_00EF;
        let jal_inst: RiscVInst<W32b> = Jal::new(RA, DataLword::from(16));
        assert_eq!(jal_inst.to_machine_code(), JAL_HEX);
    }

    #[test]
    fn test_auipc() {
        let mut state = get_init_state();
        state.set_user_pc(ByteAddr32::from(0x10FC_0000));
        state.apply_inst_test(&Auipc::new(RD, DataLword::from(0x10)));
        assert_eq!(
            u32::from(state.regfile_read(RD)),
            0x10FC_0000 + (0x10 << 12)
        );
    }

    #[test]
    fn test_lui() {
        let mut state = get_init_state();
        state.apply_inst_test(&Lui::new(RD, DataLword::from(0xD_EADC)));
        assert_eq!(u32::from(state.regfile_read(RD)), 0xDEAD_C000);
    }

    struct BTestData {
        rs1_val: DataLword,
        rs2_val: DataLword,
        should_take: bool,
    }

    /// Tests a branch instruction. Taken jumps move forward by 0x100, or backwards by 0x100.
    fn test_b_type<T: BType<W32b>>(
        state: &mut ProgramState<RiscV<W32b>, W32b>,
        args: Vec<BTestData>,
    ) {
        for &dist in &[0x100, -0x100] {
            let offs = DataLword::from(dist);
            for &BTestData {
                rs1_val,
                rs2_val,
                should_take,
            } in &args
            {
                let exp_new_pc = ByteAddr32::from(
                    i32::from(state.get_user_pc()) + if should_take { i32::from(offs) } else { 4 },
                );
                state.regfile_set(RS1, rs1_val);
                state.regfile_set(RS2, rs2_val);
                state.apply_inst_test(&T::new(RS1, RS2, offs));
                assert_eq!(exp_new_pc, state.get_user_pc());
            }
        }
    }

    #[test]
    fn test_b_type_insts() {
        let mut state: ProgramState<RiscV<W32b>, W32b> = Default::default();
        test_b_type::<Beq>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(101),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bge>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(-1),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(-1),
                    rs2_val: DataLword::from(100),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
            ],
        );
        test_b_type::<Bgeu>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(-1), // max uint
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(-1), // max uint
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
            ],
        );
        test_b_type::<Blt>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(-1),
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(-1),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bltu>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(-1), // max uint
                    rs2_val: DataLword::from(100),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(-1), // max uint
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bne>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataLword::from(-1),
                    rs2_val: DataLword::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataLword::from(100),
                    rs2_val: DataLword::from(100),
                    should_take: false,
                },
            ],
        );
    }

    #[test]
    fn test_ecall() {
        let mut state = get_init_state();
        state.regfile_set(SP, 0x7FFF_0000.into());
        let addr = ByteAddr32::from(state.regfile_read(SP));
        state.memory_set_word(addr, DataLword::from(0xDEAD_BEEFu32));
        // Set ecall code
        state.regfile_set(
            A7,
            RiscVSyscallConvention::<W32b>::syscall_to_number(Syscall::Write),
        );
        // We're writing 4 bytes to stdout, which has fd 1
        state.regfile_set(A0, DataLword::from(1));
        state.regfile_set(A1, DataLword::from(addr));
        state.regfile_set(A2, DataLword::from(4));
        let inst = Ecall::new();
        state.apply_inst_test(&inst);
        // beware of endianness
        assert_eq!(state.get_stdout(), &[0xEF, 0xBE, 0xAD, 0xDE]);
    }

    #[test]
    fn test_jal() {
        let mut state = get_init_state();
        let starting_pc_val = 0x10FC_0000;
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let inst = Jal::new(RA, DataLword::from(16));
        state.apply_inst_test(&inst);
        assert_eq!(u32::from(state.regfile_read(RA)), starting_pc_val + 4);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(starting_pc_val + 16));
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let inst = Jal::new(RA, DataLword::from(-256));
        state.apply_inst_test(&inst);
        assert_eq!(u32::from(state.regfile_read(RA)), starting_pc_val + 4);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(starting_pc_val - 256));
    }

    #[test]
    fn test_sb() {
        let mut state = get_init_state();
        let addr = 0x1000_0000;
        // black out the upper bytes to make sure it only touches the lowest word
        let byte = 0xFFFF_FFDEu32;
        state.regfile_set(RS1, DataLword::from(addr));
        state.regfile_set(RS2, DataLword::from(byte));
        // don't store msb yet
        for i in 0..3 {
            state.apply_inst_test(&Sb::new(RS1, RS2, DataLword::from(i)));
        }
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr)),
            DataLword::from(0x00DE_DEDEu32)
        );
        state.apply_inst_test(&Sb::new(RS1, RS2, DataLword::from(3)));
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr)),
            DataLword::from(0xDEDE_DEDEu32)
        );
    }

    #[test]
    fn test_sh_aligned() {
        let mut state = get_init_state();
        let test_data = vec![
            (0x1000_0000, 0, 0xABCD_0123u32),
            (0x0000_0014, 0, 0xFFEE_DDEEu32),
        ];
        for (addr, offs, rs2_val) in test_data {
            state.regfile_set(RS1, DataLword::from(addr));
            state.regfile_set(RS2_POS, DataLword::from(rs2_val));
            state.regfile_set(RS2_NEG, DataLword::from(rs2_val >> 16));
            state.apply_inst_test(&Sh::new(RS1, RS2_POS, DataLword::from(offs)));
            state.apply_inst_test(&Sh::new(RS1, RS2_NEG, DataLword::from(offs + 2)));
            assert_eq!(
                state.memory_get_word((addr + offs).into()),
                DataLword::from(rs2_val)
            );
        }
    }

    #[test]
    fn test_sw_aligned() {
        let mut state = get_init_state();
        let test_data = vec![
            (0x1000_0000, 0, DataLword::from(100)),
            (0x0000_0014, 0, DataLword::from(-4)),
        ];
        for (addr, offs, rs2_val) in test_data {
            state.regfile_set(RS1, DataLword::from(addr));
            state.regfile_set(RS2, rs2_val);
            state.apply_inst_test(&Sw::new(RS1, RS2, DataLword::from(offs)));
            assert_eq!(state.memory_get_word((addr + offs).into()), rs2_val);
        }
    }
}

/// Tests for 64-bit-only instructions.
#[cfg(test)]
mod tests_64 {
    use super::*;
    use RiscVRegister::*;

    pub fn get_init_state() -> ProgramState<RiscV<W64b>, W64b> {
        let state: ProgramState<RiscV<W64b>, W64b> = Default::default();
        state
    }

    #[test]
    fn test_sd_aligned() {
        let mut state = get_init_state();
        let val: DataDword = 0xABCD_ABCD_0123_0123u64.into();
        let addr: ByteAddr64 = 0x7FFF_FFFF_FFFF_FFF0u64.into();
        state.regfile_set(S0, val);
        state.regfile_set(S1, addr.into());
        state.apply_inst_test(&Sd::new(S1, S0, DataDword::zero()));
        assert_eq!(state.memory_get_doubleword(addr), val);
    }
}
