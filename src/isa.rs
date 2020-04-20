use crate::instruction::*;
use crate::program_state::*;

fn f3(val: u32) -> BitStr32 {
    BitStr32::new(val, 3)
}

fn f7(val: u32) -> BitStr32 {
    BitStr32::new(val, 7)
}

const R_OPCODE: BitStr32 = BitStr32::new(0b0110011, 7);
const I_OPCODE_ARITH: BitStr32 = BitStr32::new(0b0010011, 7);

pub struct Add;
impl RType for Add {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }
    fn eval(rs1_val: Word, rs2_val: Word) -> Word {
        rs1_val + rs2_val
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

    fn eval(rs1_val: Word, imm: BitStr32) -> Word {
        let imm_val = imm.as_i32();
        if imm_val >= 0 {
            rs1_val + (imm_val as u32)
        } else {
            rs1_val - ((imm_val.wrapping_abs() + 1) as u32)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::program_state::IRegister::*;
    use crate::program_state::*;

    const RS1_VAL: Word = 2;
    const RS2_VAL: Word = 49;
    const RD: IRegister = T2;
    const RS1: IRegister = T0;
    const RS2: IRegister = T1;
    const IMM: i32 = -504;

    fn get_rtype_init_state() -> ProgramState {
        let mut state = ProgramState::new();
        state.regfile.set(RS1, RS1_VAL);
        state.regfile.set(RS2, RS2_VAL);
        state
    }

    #[test]
    fn test_add() {
        let mut state = ProgramState::new();
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
