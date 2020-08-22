use super::arch::*;
use super::instruction::*;
pub use super::pseudo_inst::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::program_state::*;
use duna_macro::*;
use num_traits::ops::wrapping::{WrappingAdd, WrappingSub};

fn f3(val: u32) -> BitStr32 {
    BitStr32::new(val, 3)
}

fn f7(val: u32) -> BitStr32 {
    BitStr32::new(val, 7)
}

const R_OPCODE: BitStr32 = BitStr32::new(0b011_0011, 7);
const R_W_OPCODE: BitStr32 = BitStr32::new(0b011_1011, 7);
const I_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_0011, 7);
const I_W_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_1011, 7);
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

#[derive(ITypeArith64)]
pub struct Addiw;
impl ITypeArith<Width64b> for Addiw {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0),
            opcode: I_W_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: DataDword, imm: BitStr32) -> DataDword {
        let v1: DataWord = rs1_val.get_lower_word();
        let v2: DataWord = imm.into();
        let result: DataWord = u32::from(v1).wrapping_add(u32::from(v2)).into();
        DataDword::sign_ext_from_word(result)
    }
}

pub struct Addw;
impl RType<Width64b> for Addw {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: DataDword, rs2_val: DataDword) -> DataDword {
        let v1: DataWord = rs1_val.get_lower_word();
        let v2: DataWord = rs2_val.get_lower_word();
        let result: DataWord = u32::from(v1).wrapping_add(u32::from(v2)).into();
        DataDword::sign_ext_from_word(result)
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
        state: &UserState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
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

    fn eval(state: &ProgramState<RiscV<T>, T>) -> InstResult<RiscV<T>, T> {
        state.handle_trap(&TrapKind::Ecall)
    }
}

pub struct Jal;
impl<T: MachineDataWidth> JType<T> for Jal {
    fn inst_fields() -> JInstFields {
        JInstFields { opcode: J_OPCODE }
    }

    fn eval(
        state: &UserState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
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
        state: &ProgramState<RiscV<T>, T>,
        rd: RiscVRegister,
        rs1: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
        let v1: T::Signed = state.user_state.regfile.read(rs1).into();
        UserDiff::reg_write_op(
            &state.user_state,
            (v1.wrapping_add(&imm.into())).into(),
            rd,
            state.user_state.pc.plus_4().into(),
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

    fn eval(
        mem: &dyn Memory<T::ByteAddr>,
        addr: T::ByteAddr,
    ) -> Result<T::RegData, MemFault<T::ByteAddr>> {
        Ok(<T::RegData>::sign_ext_from_byte(mem.get_byte(addr)?))
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

    fn eval(
        mem: &dyn Memory<T::ByteAddr>,
        addr: T::ByteAddr,
    ) -> Result<T::RegData, MemFault<T::ByteAddr>> {
        Ok(<T::RegData>::zero_pad_from_byte(mem.get_byte(addr)?))
    }
}

#[derive(ITypeLoad64)]
pub struct Ld;
impl ITypeLoad<Width64b> for Ld {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b011),
        }
    }

    fn eval(
        mem: &dyn Memory<ByteAddr64>,
        addr: ByteAddr64,
    ) -> Result<DataDword, MemFault<ByteAddr64>> {
        Ok(mem.get_doubleword(addr)?)
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

    fn eval(
        mem: &dyn Memory<T::ByteAddr>,
        addr: T::ByteAddr,
    ) -> Result<T::RegData, MemFault<T::ByteAddr>> {
        Ok(<T::RegData>::sign_ext_from_half(mem.get_half(addr)?))
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

    fn eval(
        mem: &dyn Memory<T::ByteAddr>,
        addr: T::ByteAddr,
    ) -> Result<T::RegData, MemFault<T::ByteAddr>> {
        Ok(<T::RegData>::zero_pad_from_half(mem.get_half(addr)?))
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
        state: &UserState<RiscV<T>, T>,
        rd: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
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

    fn eval(
        mem: &dyn Memory<T::ByteAddr>,
        addr: T::ByteAddr,
    ) -> Result<T::RegData, MemFault<T::ByteAddr>> {
        Ok(<T::RegData>::sign_ext_from_word(mem.get_word(addr)?))
    }
}

#[derive(ITypeLoad64)]
pub struct Lwu;
impl ITypeLoad<Width64b> for Lwu {
    fn inst_fields() -> IInstFields {
        IInstFields {
            opcode: I_OPCODE_LOAD,
            funct3: f3(0b110),
        }
    }

    fn eval(
        mem: &dyn Memory<ByteAddr64>,
        addr: ByteAddr64,
    ) -> Result<DataDword, MemFault<ByteAddr64>> {
        Ok(DataDword::sign_ext_from_word(mem.get_word(addr)?))
    }
}

pub struct Or;
impl<T: MachineDataWidth> RType<T> for Or {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b110),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        (v1 | rs2_val.into()).into()
    }
}

#[derive(ITypeArith)]
pub struct Ori;
impl<T: MachineDataWidth> ITypeArith<T> for Ori {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b110),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: T::RegData, imm: BitStr32) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        let imm_val: T::Signed = imm.into();
        (v1 | imm_val).into()
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
        state: &UserState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        let new_byte = state.regfile.read(rs2).get_byte(0);
        UserDiff::mem_write_op(state, byte_addr, DataEnum::Byte(new_byte)).unwrap()
    }
}

pub struct Sd;
impl SType<Width64b> for Sd {
    fn inst_fields() -> SInstFields {
        SInstFields {
            funct3: f3(0b011),
            opcode: S_OPCODE,
        }
    }

    fn eval(
        state: &UserState<RiscV<Width64b>, Width64b>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<Width64b>, Width64b> {
        let base_addr: i64 = state.regfile.read(rs1).into();
        let byte_addr: ByteAddr64 = (base_addr.wrapping_add(imm.into())).into();
        let new_dword = state.regfile.read(rs2);
        UserDiff::mem_write_op(state, byte_addr, DataEnum::DoubleWord(new_dword)).unwrap()
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
        state: &UserState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        let lower_byte: u8 = state.regfile.read(rs2).get_byte(0).into();
        let upper_byte: u8 = state.regfile.read(rs2).get_byte(1).into();
        let full: u16 = ((upper_byte as u16) << 8) | (lower_byte as u16);
        UserDiff::mem_write_op(state, byte_addr, DataEnum::Half(full.into())).unwrap()
    }
}

pub struct Sub;
impl<T: MachineDataWidth> RType<T> for Sub {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData {
        let v1: T::Unsigned = rs1_val.into();
        (v1.wrapping_sub(&rs2_val.into())).into()
    }
}

pub struct Subw;
impl RType<Width64b> for Subw {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0b0100_0000),
            funct3: f3(0),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: DataDword, rs2_val: DataDword) -> DataDword {
        let v1: DataWord = rs1_val.get_lower_word();
        let v2: DataWord = rs2_val.get_lower_word();
        let result: DataWord = u32::from(v1).wrapping_sub(u32::from(v2)).into();
        DataDword::sign_ext_from_word(result)
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
        state: &UserState<RiscV<T>, T>,
        rs1: RiscVRegister,
        rs2: RiscVRegister,
        imm: BitStr32,
    ) -> InstResult<RiscV<T>, T> {
        let base_addr: T::Signed = state.regfile.read(rs1).into();
        let byte_addr: T::ByteAddr = (base_addr.wrapping_add(&imm.into())).into();
        UserDiff::mem_write_op(
            state,
            byte_addr,
            DataEnum::Word(state.regfile.read(rs2).get_lower_word()),
        )
        .unwrap()
    }
}

pub struct Xor;
impl<T: MachineDataWidth> RType<T> for Xor {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b100),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: T::RegData, rs2_val: T::RegData) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        (v1 ^ rs2_val.into()).into()
    }
}

#[derive(ITypeArith)]
pub struct Xori;
impl<T: MachineDataWidth> ITypeArith<T> for Xori {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b100),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: T::RegData, imm: BitStr32) -> T::RegData {
        let v1: T::Signed = rs1_val.into();
        let imm_val: T::Signed = imm.into();
        (v1 ^ imm_val).into()
    }
}

/// Tests for the base 32-bit ISA.
#[cfg(test)]
mod tests_32 {
    use super::super::program::RiscVSyscallConvention;
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
        let mut state: ProgramState<RiscV<Width32b>, Width32b> = Default::default();
        state.regfile_set(RS1, DataWord::from(RS1_VAL));
        state.regfile_set(RS2_POS, DataWord::from(RS2_VAL_POS));
        state.regfile_set(RS2_NEG, DataWord::from(RS2_VAL_NEG));
        state
    }

    #[test]
    fn test_write_x0() {
        let mut state: ProgramState<RiscV<Width32b>, Width32b> = Default::default();
        state.apply_inst_test(&Addi::new(ZERO, ZERO, DataWord::from(0x100)));
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
            state.apply_inst_test(&T::new(RD, RS1, rs2));
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
            state.apply_inst_test(&T::new(RD, RS1, DataWord::from(imm)));
            println!("{:b}\n{:b}", i32::from(state.regfile_read(RD)), result);
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
    fn test_auipc() {
        let mut state = get_init_state();
        state.set_user_pc(ByteAddr32::from(0x10FC_0000));
        state.apply_inst_test(&Auipc::new(RD, DataWord::from(0x10)));
        assert_eq!(
            u32::from(state.regfile_read(RD)),
            0x10FC_0000 + (0x10 << 12)
        );
    }

    #[test]
    fn test_lui() {
        let mut state = get_init_state();
        state.apply_inst_test(&Lui::new(RD, DataWord::from(0xD_EADC)));
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
                state.apply_inst_test(&T::new(RS1, RS2, offs));
                assert_eq!(exp_new_pc, state.get_user_pc());
            }
        }
    }

    #[test]
    fn test_b_type_insts() {
        let mut state: ProgramState<RiscV<Width32b>, Width32b> = Default::default();
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

    #[test]
    fn test_ecall() {
        let mut state = get_init_state();
        state.regfile_set(SP, 0x7FFF_000.into());
        let addr = ByteAddr32::from(state.regfile_read(SP));
        state.memory_set_word(addr, DataWord::from(0xDEAD_BEEFu32));
        // Set ecall code
        state.regfile_set(
            A7,
            RiscVSyscallConvention::<Width32b>::syscall_to_number(Syscall::Write),
        );
        // We're writing 4 bytes to stdout, which has fd 1
        state.regfile_set(A0, DataWord::from(1));
        state.regfile_set(A1, DataWord::from(addr));
        state.regfile_set(A2, DataWord::from(4));
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
        let inst = Jal::new(RA, DataWord::from(16));
        state.apply_inst_test(&inst);
        assert_eq!(u32::from(state.regfile_read(RA)), starting_pc_val + 4);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(starting_pc_val + 16));
        state.set_user_pc(ByteAddr32::from(starting_pc_val));
        let inst = Jal::new(RA, DataWord::from(-256));
        state.apply_inst_test(&inst);
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
        state.apply_inst_test(&inst);
        assert_eq!(state.get_user_pc(), ByteAddr32::from(exp_pc_val));
    }

    #[test]
    fn test_lb_lbu() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(ByteAddr32::from(base_addr), DataWord::from(test_data));
        state.regfile_set(T0, DataWord::from(base_addr));
        // signed loads
        state.apply_inst_test(&Lb::new(T1, T0, DataWord::from(0)));
        state.apply_inst_test(&Lb::new(T2, T0, DataWord::from(1)));
        state.apply_inst_test(&Lb::new(T3, T0, DataWord::from(2)));
        state.apply_inst_test(&Lb::new(T4, T0, DataWord::from(3)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0x01));
        assert_eq!(state.regfile_read(T2), DataWord::from(0xFFFF_FFEFu32));
        assert_eq!(state.regfile_read(T3), DataWord::from(0xFFFF_FFCDu32));
        assert_eq!(state.regfile_read(T4), DataWord::from(0xFFFF_FFABu32));
        // unsigned loads
        state.apply_inst_test(&Lbu::new(T1, T0, DataWord::from(0)));
        state.apply_inst_test(&Lbu::new(T2, T0, DataWord::from(1)));
        state.apply_inst_test(&Lbu::new(T3, T0, DataWord::from(2)));
        state.apply_inst_test(&Lbu::new(T4, T0, DataWord::from(3)));
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
        state.memory_set_word(ByteAddr32::from(base_addr), DataWord::from(test_data));
        state.regfile_set(T0, DataWord::from(base_addr));
        // signed loads
        state.apply_inst_test(&Lh::new(T1, T0, DataWord::from(0)));
        state.apply_inst_test(&Lh::new(T2, T0, DataWord::from(2)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0xFFFF_EF01u32));
        assert_eq!(state.regfile_read(T2), DataWord::from(0x0BCD));
        // unsigned loads
        state.apply_inst_test(&Lhu::new(T1, T0, DataWord::from(0)));
        state.apply_inst_test(&Lhu::new(T2, T0, DataWord::from(2)));
        assert_eq!(state.regfile_read(T1), DataWord::from(0xEF01));
        assert_eq!(state.regfile_read(T2), DataWord::from(0x0BCD));
    }

    #[test]
    fn test_lw_aligned() {
        let mut state = get_init_state();
        let base_addr = 0xFFFF_0004u32;
        let test_data = 0xABCD_EF01u32;
        state.memory_set_word(ByteAddr32::from(base_addr), DataWord::from(test_data));
        state.regfile_set(T0, DataWord::from(base_addr));
        state.apply_inst_test(&Lw::new(T1, T0, DataWord::from(0)));
        assert_eq!(state.regfile_read(T1), DataWord::from(test_data));
        // Test loading with negative offset
        state.regfile_set(T0, DataWord::from(base_addr + 16));
        state.apply_inst_test(&Lw::new(T1, T0, DataWord::from(-16)));
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
            state.apply_inst_test(&Sb::new(RS1, RS2, DataWord::from(i)));
        }
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr)),
            DataWord::from(0x00DE_DEDEu32)
        );
        state.apply_inst_test(&Sb::new(RS1, RS2, DataWord::from(3)));
        assert_eq!(
            state.memory_get_word(ByteAddr32::from(addr)),
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
            state.apply_inst_test(&Sh::new(RS1, RS2_POS, DataWord::from(offs)));
            state.apply_inst_test(&Sh::new(RS1, RS2_NEG, DataWord::from(offs + 2)));
            assert_eq!(
                state.memory_get_word((addr + offs).into()),
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
            state.apply_inst_test(&Sw::new(RS1, RS2, DataWord::from(offs)));
            assert_eq!(state.memory_get_word((addr + offs).into()), rs2_val);
        }
    }
}

/// Tests for 64-bit-only instructions.
#[cfg(test)]
mod tests_64 {
    use super::*;
    use RiscVRegister::*;

    fn get_init_state() -> ProgramState<RiscV<Width64b>, Width64b> {
        let state: ProgramState<RiscV<Width64b>, Width64b> = Default::default();
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
        let val: DataWord = 0xF123_0123u32.into();
        let addr: ByteAddr64 = 0x7FFF_FFFF_FFFF_FFF0u64.into();
        state.memory_set_word(addr, val);
        state.regfile_set(S1, addr.into());
        state.apply_inst_test(&Lwu::new(S2, S1, DataDword::zero()));
        assert_eq!(state.regfile_read(S2), DataDword::sign_ext_from_word(val));
        // this value will not be sign extended
        let val2: DataWord = 0x0123_0123u32.into();
        state.memory_set_word(addr, val2);
        state.regfile_set(T1, addr.into());
        state.apply_inst_test(&Lwu::new(T2, T1, DataDword::zero()));
        assert_eq!(state.regfile_read(T2), DataDword::zero_pad_from_word(val2));
    }

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
