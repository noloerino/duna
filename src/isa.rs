use crate::instruction::*;
use crate::program_state::*;

fn f3(val: u32) -> BitStr32 {
    BitStr32::new(val, 3)
}

fn f7(val: u32) -> BitStr32 {
    BitStr32::new(val, 7)
}

const R_OPCODE: BitStr32 = BitStr32::new(0b011_0011, 7);
const I_OPCODE_ARITH: BitStr32 = BitStr32::new(0b001_0011, 7);

pub struct Add;
impl RType for Add {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }
    fn eval(rs1_val: DataWord, rs2_val: DataWord) -> DataWord {
        DataWord::from(i32::from(rs1_val) + i32::from(rs2_val))
    }
}

pub struct Addi;
impl ITypeArith for Addi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: DataWord, imm: BitStr32) -> DataWord {
        DataWord::from(i32::from(rs1_val) + imm.as_i32())
    }
}

#[cfg(test)]
mod test {
    use crate::isa::*;
    use crate::program_state::IRegister::*;

    const RS1_VAL: i32 = 1023;
    const RS2_VAL_POS: i32 = 49;
    const RD: IRegister = T2;
    const RS1: IRegister = T0;
    const RS2_POS: IRegister = T1;
    const RS2_NEG: IRegister = S1;

    fn get_init_state() -> ProgramState {
        let mut state = ProgramState::new();
        state.regfile.set(RS1, DataWord::from(RS1_VAL));
        state.regfile.set(RS2_POS, DataWord::from(RS2_VAL_POS));
        state.regfile.set(RS2_NEG, DataWord::from(-1i32));
        state
    }

    #[test]
    fn test_to_binary() {
        // add s0, s1, s2
        const ADD_HEX: u32 = 0x0124_8433;
        assert_eq!(Add::new(S0, S1, S2).to_machine_code(), ADD_HEX);
    }

    #[test]
    fn test_add() {
        let mut state = get_init_state();
        state.apply_inst(&Add::new(RD, RS1, RS2_POS));
        assert_eq!(
            i32::from(state.regfile.read(RD)),
            i32::from(RS1_VAL) + i32::from(RS2_VAL_POS)
        );
        state.apply_inst(&Add::new(RD, RS1, RS2_NEG));
        assert_eq!(i32::from(state.regfile.read(RD)), i32::from(RS1_VAL) - 1);
    }

    #[test]
    fn test_addi() {
        let mut state = get_init_state();
        state.apply_inst(&Addi::new(RD, RS1, -504i32 as u32));
        assert_eq!(i32::from(state.regfile.read(RD)), i32::from(RS1_VAL) - 504);
        state.apply_inst(&Addi::new(RD, RS1, -1i32 as u32));
        assert_eq!(i32::from(state.regfile.read(RD)), i32::from(RS1_VAL) - 1);
        state.apply_inst(&Addi::new(RD, RS1, -1024i32 as u32));
        assert_eq!(i32::from(state.regfile.read(RD)), i32::from(RS1_VAL) - 1024);
    }
}

#[allow(dead_code)]
pub enum Instruction {
    Add,
    Addi,
    And,
    Andi,
    Auipc,
    Beq,
    Bge,
    Bgeu,
    Blt,
    Bltu,
    Bne,
    Ebreak,
    Ecall,
    Jal,
    Jalr,
    Lb,
    Lbu,
    Ld,
    Lh,
    Lhu,
    Lui,
    Lw,
    Lwu,
    Or,
    Ori,
    Sb,
    Sd,
    Sh,
    Sll,
    Slli,
    Slt,
    Slti,
    Sltiu,
    Sltu,
    Sra,
    Srai,
    Srl,
    Srli,
    Sub,
    Sw,
    Xor,
    Xori,
}
