use super::arch::*;
use super::instruction::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::program_state::*;
use duna_macro::*;
use num_traits::ops::wrapping::WrappingAdd;

fn f3(val: u32) -> BitStr32 {
    BitStr32::new(val, 3)
}

fn f7(val: u32) -> BitStr32 {
    BitStr32::new(val, 7)
}

const R_OPCODE: BitStr32 = BitStr32::new(0b011_0011, 7);
const I_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_0011, 7);
const I_OPCODE_LOAD: BitStr32 = BitStr32::new(0b000_0011, 7);
const SYS_OPCODE: BitStr32 = BitStr32::new(0b111_0011, 7);
const B_OPCODE: BitStr32 = BitStr32::new(0b110_0011, 7);
const J_OPCODE: BitStr32 = BitStr32::new(0b110_1111, 7);
const S_OPCODE: BitStr32 = BitStr32::new(0b010_0011, 7);

pub struct Add;
impl<T: MachineDataWidth> RType<T> for Add {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }
    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        (v1.wrapping_add(&rs2_val.into())).into()
    }
}

#[derive(ITypeArith)]
pub struct Addi;
impl<T: MachineDataWidth> ITypeArith<T> for Addi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: T::RegData, imm: BitStr32) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        let imm_val: T::Signed = imm.into();
        (v1.wrapping_add(&imm_val)).into()
    }
}

pub struct And;
impl<T: MachineDataWidth> RType<T> for And {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b111),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        (v1 & rs2_val.into()).into()
    }
}

#[derive(ITypeArith)]
pub struct Andi;
impl<T: MachineDataWidth> ITypeArith<T> for Andi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b111),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: T::RegData, imm: BitStr32) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        let imm_val: T::Signed = imm.into();
        (v1 & imm_val).into()
    }
}

pub struct Auipc;
impl<T: MachineDataWidth> UType<T> for Auipc {
    fn inst_fields() -> UInstFields {
        UInstFields {
            opcode: BitStr32::new(0b001_0111, 7),
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        let pc: T::Signed = state.pc.into();
        UserDiff::reg_write_pc_p4(
            state,
            rd,
            (pc.wrapping_add(&imm.zero_pad_lsb().into())).into(),
        )
    }
}

pub struct Beq;
impl<T: MachineDataWidth> BType<T> for Beq {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        rs1_val == rs2_val
    }
}

pub struct Bge;
impl<T: MachineDataWidth> BType<T> for Bge {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b101),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        let v1: T::Signed = rs1_val.into();
        v1 >= rs2_val.into()
    }
}

pub struct Bgeu;
impl<T: MachineDataWidth> BType<T> for Bgeu {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b111),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        let v1: T::Unsigned = rs1_val.into();
        v1 >= rs2_val.into()
    }
}

pub struct Blt;
impl<T: MachineDataWidth> BType<T> for Blt {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b100),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        let v1: T::Signed = rs1_val.into();
        v1 < rs2_val.into()
    }
}

pub struct Bltu;
impl<T: MachineDataWidth> BType<T> for Bltu {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b110),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        let v1: T::Unsigned = rs1_val.into();
        v1 < rs2_val.into()
    }
}

pub struct Bne;
impl<T: MachineDataWidth> BType<T> for Bne {
    fn inst_fields() -> BInstFields {
        BInstFields {
            opcode: B_OPCODE,
            funct3: f3(0b001),
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> bool {
        rs1_val != rs2_val
    }
}

pub struct Ecall;
impl<T: MachineDataWidth> EnvironInst<T> for Ecall {
    fn funct12() -> BitStr32 {
        BitStr32::new(0, 12)
    }

    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: SYS_OPCODE,
            funct3: f3(0b000),
        }
    }

    fn eval(_state: &ProgramState<RiscV<T>, T>) -> TrapKind {
        TrapKind::Ecall
    }
}

pub struct Jal;
impl<T: MachineDataWidth> JType<T> for Jal {
    fn inst_fields() -> JInstFields {
        JInstFields { opcode: J_OPCODE }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        let pc: T::Signed = state.pc.into();
        let offs: T::Signed = imm.into();
        UserDiff::reg_write_op(
            state,
            (pc.wrapping_add(&offs)).into(),
            rd,
            state.pc.plus_4().into(),
        )
    }
}

pub struct Jalr;
impl<T: MachineDataWidth> IType<T> for Jalr {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: BitStr32::new(0b110_0111, 7),
            funct3: f3(0b000),
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        let v1: T::Signed = state.regfile.read(rs1).into();
        UserDiff::reg_write_op(
            state,
            (v1.wrapping_add(&imm.into())).into(),
            rd,
            state.pc.plus_4().into(),
        )
    }
}

#[derive(ITypeLoad)]
pub struct Lb;
impl<T: MachineDataWidth> ITypeLoad<T> for Lb {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b000),
        }
    }

    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData {
        <T::RegData>::sign_ext_from_byte(mem.get_byte(addr))
    }
}

#[derive(ITypeLoad)]
pub struct Lbu;
impl<T: MachineDataWidth> ITypeLoad<T> for Lbu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b100),
        }
    }

    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData {
        <T::RegData>::zero_pad_from_byte(mem.get_byte(addr))
    }
}

#[derive(ITypeLoad)]
pub struct Lh;
impl<T: MachineDataWidth> ITypeLoad<T> for Lh {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b001),
        }
    }

    // TODO define alignment behavior
    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData {
        let og_addr: T::Signed = addr.into();
        let second_byte_addr: T::ByteAddr = (og_addr.wrapping_add(&T::sgn_one())).into();
        let upper_byte: T::Signed =
            <T::RegData>::sign_ext_from_byte(mem.get_byte(second_byte_addr)).into();
        let lower_byte: T::Signed = <T::RegData>::zero_pad_from_byte(mem.get_byte(addr)).into();
        ((upper_byte << 8) | lower_byte).into()
    }
}

#[derive(ITypeLoad)]
pub struct Lhu;
impl<T: MachineDataWidth> ITypeLoad<T> for Lhu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b101),
        }
    }

    // TODO define alignment behavior
    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData {
        let og_addr: T::Signed = addr.into();
        let second_byte_addr: T::ByteAddr = (og_addr.wrapping_add(&T::sgn_one())).into();
        let upper_byte: T::Signed =
            <T::RegData>::zero_pad_from_byte(mem.get_byte(second_byte_addr)).into();
        let lower_byte: T::Signed = <T::RegData>::zero_pad_from_byte(mem.get_byte(addr)).into();
        ((upper_byte << 8) | lower_byte).into()
    }
}

pub struct Lui;
impl<T: MachineDataWidth> UType<T> for Lui {
    fn inst_fields() -> UInstFields {
        UInstFields {
            opcode: BitStr32::new(0b011_0111, 7),
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        let imm_val: T::Signed = imm.zero_pad_lsb().into();
        UserDiff::reg_write_pc_p4(state, rd, imm_val.into())
    }
}

#[derive(ITypeLoad)]
pub struct Lw;
impl<T: MachineDataWidth> ITypeLoad<T> for Lw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b010),
        }
    }

    // TODO define alignment behavior
    fn eval(mem: &Memory<T>, addr: T::ByteAddr) -> T::RegData {
        <T::RegData>::sign_ext_from_word(mem.get_word(addr.to_word_address()))
    }
}

pub struct Sb;
impl<T: MachineDataWidth> SType<T> for Sb {
    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b000),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        // TODO implement more granular diffs
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        let new_word = state.memory.get_word(byte_addr.to_word_address()).set_byte(
            byte_addr.get_word_offset(),
            state.regfile.read(rs2).get_byte(0),
        );
        UserDiff::mem_write_op(state, byte_addr.to_word_address(), new_word)
    }
}

pub struct Sh;
impl<T: MachineDataWidth> SType<T> for Sh {
    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b001),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        // TODO implement more granular diffs
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        let new_word = state
            .memory
            .get_word(byte_addr.to_word_address())
            .set_byte(
                byte_addr.get_word_offset(),
                state.regfile.read(rs2).get_byte(0),
            )
            .set_byte(
                // will panic if not properly aligned, but we don't care yet
                byte_addr.get_word_offset().wrapping_add(1),
                state.regfile.read(rs2).get_byte(1),
            );
        UserDiff::mem_write_op(state, byte_addr.to_word_address(), new_word)
    }
}

pub struct Sw;
impl<T: MachineDataWidth> SType<T> for Sw {
    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b010),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &UserProgState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> UserDiff<RiscV<T>, T> {
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        UserDiff::mem_write_op(
            state,
            byte_addr.to_word_address(),
            state.regfile.read(rs2).get_lower_word(),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::instruction::*;
    use crate::program_state::Syscall;
    use RiscVRegister::*;

    const RS1_VAL: i32 = 1023;
    const RS2_VAL_POS: i32 = 49;
    const RS2_VAL_NEG: i32 = -1;
    const RD: RiscVRegister = T2;
    const RS1: RiscVRegister = T0;
    const RS2: RiscVRegister = T3;
    const RS2_POS: RiscVRegister = T1;
    const RS2_NEG: RiscVRegister = S1;

    fn get_init_state() -> ProgramState<RiscV<Width32b>, Width32b> {
        let mut state = ProgramState::new();
        state.regfile_set(RS1, DataWord::from(RS1_VAL));
        state.regfile_set(RS2_POS, DataWord::from(RS2_VAL_POS));
        state.regfile_set(RS2_NEG, DataWord::from(RS2_VAL_NEG));
        state
    }

    #[test]
    fn test_write_x0() {
        let mut state = ProgramState::<RiscV<Width32b>, Width32b>::new();
        state.apply_inst(&Addi::new(ZERO, ZERO, DataWord::from(0x100)));
        assert_eq!(i32::from(state.regfile_read(ZERO)), 0);
    }

    struct RTestData {
        rs2: RiscVRegister,
        result: i32,
    }

    /// Tests an R type instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_r_type<T: RType<Width32b>>(
        state: &mut ProgramState<RiscV<Width32b>, Width32b>,
        args: Vec<RTestData>,
    ) {
        for RTestData { rs2, result } in args {
            state.apply_inst(&T::new(RD, RS1, rs2));
            assert_eq!(i32::from(state.regfile_read(RD)), result);
        }
    }

    struct IArithTestData {
        imm: i32,
        result: i32,
    }

    /// Tests an I type arithmetic instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_i_type_arith<T: ITypeArith<Width32b>>(
        state: &mut ProgramState<RiscV<Width32b>, Width32b>,
        args: Vec<IArithTestData>,
    ) {
        for IArithTestData { imm, result } in args {
            state.apply_inst(&T::new(RD, RS1, DataWord::from(imm)));
            assert_eq!(i32::from(state.regfile_read(RD)), result);
        }
    }

    #[test]
    /// Tests machine code conversions of instructions.
    /// These conversions were done by typing the instruction into Venus.
    fn test_to_machine_code() {
        // add s0, s1, s2
        const ADD_HEX: u32 = 0x0124_8433;
        let add_inst: RiscVInst<Width32b> = Add::new(RiscVRegister::S0, S1, S2);
        assert_eq!(add_inst.to_machine_code(), ADD_HEX);
        // addi T1, T1, -1075
        const ADDI_HEX: u32 = 0xBCD3_0313;
        let addi_inst: RiscVInst<Width32b> = Addi::new(T1, T1, DataWord::from(-1075));
        assert_eq!(addi_inst.to_machine_code(), ADDI_HEX);
        // auipc s1, 10
        const AUIPC_HEX: u32 = 0x0000_A497;
        let auipc_inst: RiscVInst<Width32b> = Auipc::new(S1, DataWord::from(10));
        assert_eq!(auipc_inst.to_machine_code(), AUIPC_HEX);
        // bne s1, s2, 4
        const BNE_HEX: u32 = 0x0124_9263;
        let bne_inst: RiscVInst<Width32b> = Bne::new(S1, S2, DataWord::from(4));
        assert_eq!(bne_inst.to_machine_code(), BNE_HEX);
        // ecall
        const ECALL_HEX: u32 = 0x0000_0073;
        let ecall_inst: RiscVInst<Width32b> = Ecall::new();
        assert_eq!(ecall_inst.to_machine_code(), ECALL_HEX);
        // jal ra, 16
        const JAL_HEX: u32 = 0x0100_00EF;
        let jal_inst: RiscVInst<Width32b> = Jal::new(RA, DataWord::from(16));
        assert_eq!(jal_inst.to_machine_code(), JAL_HEX);
    }

    #[test]
    /// The RISCV spec defines all arithmetic to be wrapping.
    fn test_add_overflow() {
        let mut state = get_init_state();
        let rs1_val = 11_0729_6010_i32;
        let rs2_val = 12_4243_4058_i32;
        state.regfile_set(RS1, DataWord::from(rs1_val));
        state.regfile_set(RS2_POS, DataWord::from(rs2_val));
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
        )
    }

    #[test]
    fn test_i_type_arith_insts() {
        let mut state = get_init_state();
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
        test_i_type_arith::<Andi>(
            &mut state,
            vec![
                IArithTestData {
                    imm: 0b1100,
                    result: RS1_VAL & 0b1100,
                },
                IArithTestData {
                    imm: 0xFF7C_FABCu32 as i32,
                    result: RS1_VAL & 0xFF7C_FABCu32 as i32,
                },
            ],
        )
    }

    #[test]
    fn test_auipc() {
        let mut state = get_init_state();
        state.set_user_pc(ByteAddr32::from(0x10FC_0000));
        state.apply_inst(&Auipc::new(RD, DataWord::from(0x10)));
        assert_eq!(
            u32::from(state.regfile_read(RD)),
            0x10FC_0000 + (0x10 << 12)
        );
    }

    #[test]
    fn test_lui() {
        let mut state = get_init_state();
        state.apply_inst(&Lui::new(RD, DataWord::from(0xD_EADC)));
        assert_eq!(u32::from(state.regfile_read(RD)), 0xDEAD_C000);
    }

    struct BTestData {
        rs1_val: DataWord,
        rs2_val: DataWord,
        should_take: bool,
    }

    /// Tests a branch instruction. Taken jumps move forward by 0x100, or backwards by 0x100.
    fn test_b_type<T: BType<Width32b>>(
        state: &mut ProgramState<RiscV<Width32b>, Width32b>,
        args: Vec<BTestData>,
    ) {
        for &dist in &[0x100, -0x100] {
            let offs = DataWord::from(dist);
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
                state.apply_inst(&T::new(RS1, RS2, offs));
                assert_eq!(exp_new_pc, state.get_user_pc());
            }
        }
    }

    #[test]
    fn test_b_type_insts() {
        let mut state = ProgramState::new();
        test_b_type::<Beq>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(101),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bge>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(-1),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(-1),
                    rs2_val: DataWord::from(100),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
            ],
        );
        test_b_type::<Bgeu>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(-1), // max uint
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(-1), // max uint
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
            ],
        );
        test_b_type::<Blt>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(-1),
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(-1),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bltu>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(-1), // max uint
                    rs2_val: DataWord::from(100),
                    should_take: false,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(-1), // max uint
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: false,
                },
            ],
        );
        test_b_type::<Bne>(
            &mut state,
            vec![
                BTestData {
                    rs1_val: DataWord::from(-1),
                    rs2_val: DataWord::from(100),
                    should_take: true,
                },
                BTestData {
                    rs1_val: DataWord::from(100),
                    rs2_val: DataWord::from(100),
                    should_take: false,
                },
            ],
        );
    }

    /* TODO bring this back
    fn test_ecall() {
        let mut state = get_init_state();
        let addr = ByteAddr32::from(state.regfile_read(SP));
        state.memory_set_word(addr.to_word_address(), DataWord::from(0xDEAD_BEEFu32));
        // Set ecall code
        state.regfile_set(A7, RV32SyscallTable::syscall_to_number(Syscall::Write)); // We're writing 4 bytes to stdout, which has fd 1
        state.regfile_set(A0, DataWord::from(1));
        state.regfile_set(A1, DataWord::from(addr));
        state.regfile_set(A2, DataWord::from(4));
        let inst = Ecall::new();
        state.apply_inst(&inst);
        // beware of endianness
        assert_eq!(state.get_stdout(), &[0xEF, 0xBE, 0xAD, 0xDE]);
    }
    */

    #[test]
    fn test_jal() {
        let mut state = get_init_state();
        let starting_pc_val = 0x10FC_0000;
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let inst = Jal::new(RA, DataWord::from(16));
        state.apply_inst(&inst);
        assert_eq!(u32::from(state.regfile_read(RA)), starting_pc_val + 4);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(starting_pc_val + 16));
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let inst = Jal::new(RA, DataWord::from(-256));
        state.apply_inst(&inst);
        assert_eq!(u32::from(state.regfile_read(RA)), starting_pc_val + 4);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(starting_pc_val - 256));
    }

    #[test]
    fn test_jalr() {
        let mut state = get_init_state();
        let starting_pc_val = 0x10FC_0000;
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let tgt_pc_val = 0xABCD_EF10u32;
        let inst = Jalr::new(RA, RS1, DataWord::from(-4));
        let exp_pc_val = tgt_pc_val - 4;
        state.regfile_set(RS1, DataWord::from(tgt_pc_val));
        state.apply_inst(&inst);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(exp_pc_val));
    }

    #[test]
    fn test_lb_lbu() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(
            ByteAddr32::from(base_addr).to_word_address(),
            DataWord::from(test_data),
        );
        state.regfile_set(T0, DataWord::from(base_addr));
        // signed loads
        state.apply_inst(&Lb::new(T1, T0, DataWord::from(0)));
        state.apply_inst(&Lb::new(T2, T0, DataWord::from(1)));
        state.apply_inst(&Lb::new(T3, T0, DataWord::from(2)));
        state.apply_inst(&Lb::new(T4, T0, DataWord::from(3)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0x01));
        assert_eq!(state.regfile_read(T2), DataWord::from(0xFFFF_FFEFu32));
        assert_eq!(state.regfile_read(T3), DataWord::from(0xFFFF_FFCDu32));
        assert_eq!(state.regfile_read(T4), DataWord::from(0xFFFF_FFABu32));
        // unsigned loads
        state.apply_inst(&Lbu::new(T1, T0, DataWord::from(0)));
        state.apply_inst(&Lbu::new(T2, T0, DataWord::from(1)));
        state.apply_inst(&Lbu::new(T3, T0, DataWord::from(2)));
        state.apply_inst(&Lbu::new(T4, T0, DataWord::from(3)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0x01));
        assert_eq!(state.regfile_read(T2), DataWord::from(0xEF));
        assert_eq!(state.regfile_read(T3), DataWord::from(0xCD));
        assert_eq!(state.regfile_read(T4), DataWord::from(0xAB));
    }

    #[test]
    fn test_lh_lhu_aligned() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0x0BCD_EF01u32;
        state.memory_set_word(
            ByteAddr32::from(base_addr).to_word_address(),
            DataWord::from(test_data),
        );
        state.regfile_set(T0, DataWord::from(base_addr));
        // signed loads
        state.apply_inst(&Lh::new(T1, T0, DataWord::from(0)));
        state.apply_inst(&Lh::new(T2, T0, DataWord::from(2)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0xFFFF_EF01u32));
        assert_eq!(state.regfile_read(T2), DataWord::from(0x0BCD));
        // unsigned loads
        state.apply_inst(&Lhu::new(T1, T0, DataWord::from(0)));
        state.apply_inst(&Lhu::new(T2, T0, DataWord::from(2)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0xEF01));
        assert_eq!(state.regfile_read(T2), DataWord::from(0x0BCD));
    }

    #[test]
    fn test_lw_aligned() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(
            ByteAddr32::from(base_addr).to_word_address(),
            DataWord::from(test_data),
        );
        state.regfile_set(T0, DataWord::from(base_addr));
        state.apply_inst(&Lw::new(T1, T0, DataWord::from(0)));
        assert_eq!(state.regfile_read(T1), DataWord::from(test_data));
        // Test loading with negative offset
        state.regfile_set(T0, DataWord::from(base_addr + 16));
        state.apply_inst(&Lw::new(T1, T0, DataWord::from(-16)));
        assert_eq!(state.regfile_read(T1), DataWord::from(test_data));
    }

    #[test]
    fn test_sb() {
        let mut state = get_init_state();
        let addr = 0x1000_0000;
        // black out the upper bytes to make sure it only touches the lowest word
        let byte = 0xFFFF_FFDEu32;
        state.regfile_set(RS1, DataWord::from(addr));
        state.regfile_set(RS2, DataWord::from(byte));
        // don't store msb yet
        for i in 0..3 {
            state.apply_inst(&Sb::new(RS1, RS2, DataWord::from(i)));
        }
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr).to_word_address()),
            DataWord::from(0x00DE_DEDEu32)
        );
        state.apply_inst(&Sb::new(RS1, RS2, DataWord::from(3)));
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr).to_word_address()),
            DataWord::from(0xDEDE_DEDEu32)
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
            state.regfile_set(RS1, DataWord::from(addr));
            state.regfile_set(RS2_POS, DataWord::from(rs2_val));
            state.regfile_set(RS2_NEG, DataWord::from(rs2_val >> 16));
            state.apply_inst(&Sh::new(RS1, RS2_POS, DataWord::from(offs)));
            state.apply_inst(&Sh::new(RS1, RS2_NEG, DataWord::from(offs + 2)));
            assert_eq!(
                state.memory_get_word(((addr + offs) >> 2) as u32),
                DataWord::from(rs2_val)
            );
        }
    }

    #[test]
    fn test_sw_aligned() {
        let mut state = get_init_state();
        let test_data = vec![
            (0x1000_0000, 0, DataWord::from(100)),
            (0x0000_0014, 0, DataWord::from(-4)),
        ];
        for (addr, offs, rs2_val) in test_data {
            state.regfile_set(RS1, DataWord::from(addr));
            state.regfile_set(RS2, rs2_val);
            state.apply_inst(&Sw::new(RS1, RS2, DataWord::from(offs)));
            assert_eq!(state.memory_get_word(((addr + offs) >> 2) as u32), rs2_val);
        }
    }
}

// pub enum Instruction {
//     Addw,
//     Addiw,
//     Ebreak,
//     Ld,
//     Lwu,
//     Or,
//     Ori,
//     Sd,
//     Sll,
//     Sllw,
//     Slli,
//     Slt,
//     Slti,
//     Sltiu,
//     Sltu,
//     Sra,
//     Srai,
//     Sraiw,
//     Sraw,
//     Srl,
//     Srli,
//     Srlis,
//     Srlw,
//     Sub,
//     Subw,
//     Xor,
//     Xori,
// }
