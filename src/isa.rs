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

pub struct And;
impl RType for And {
    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(0),
            funct3: f3(0b111),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: DataWord, rs2_val: DataWord) -> DataWord {
        DataWord::from(i32::from(rs1_val) & i32::from(rs2_val))
    }
}

pub struct Andi;
impl ITypeArith for Andi {
    fn inst_fields() -> IInstFields {
        IInstFields {
            funct3: f3(0b111),
            opcode: I_OPCODE_ARITH,
        }
    }

    fn eval(rs1_val: DataWord, imm: BitStr32) -> DataWord {
        DataWord::from(i32::from(rs1_val) & imm.as_i32())
    }
}

pub struct Auipc;
impl UType for Auipc {
    fn inst_fields() -> UInstFields {
        UInstFields {
            opcode: BitStr32::new(0b001_0111, 7),
        }
    }

    fn eval(state: &ProgramState, rd: IRegister, imm: BitStr32) -> StateChange {
        StateChange::reg_write_pc_p4(
            state,
            rd,
            DataWord::from(u32::from(state.pc) + imm.zero_pad_lsb().as_u32()),
        )
    }
}

#[cfg(test)]
mod test {
    use crate::isa::*;
    use crate::program_state::IRegister::*;

    const RS1_VAL: i32 = 1023;
    const RS2_VAL_POS: i32 = 49;
    const RS2_VAL_NEG: i32 = -1;
    const RD: IRegister = T2;
    const RS1: IRegister = T0;
    const RS2_POS: IRegister = T1;
    const RS2_NEG: IRegister = S1;

    fn get_init_state() -> ProgramState {
        let mut state = ProgramState::new();
        state.regfile.set(RS1, DataWord::from(RS1_VAL));
        state.regfile.set(RS2_POS, DataWord::from(RS2_VAL_POS));
        state.regfile.set(RS2_NEG, DataWord::from(RS2_VAL_NEG));
        state
    }

    struct RTestData {
        rs2: IRegister,
        result: i32,
    }

    /// Tests an R type instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_r_type<T: RType>(state: &mut ProgramState, args: Vec<RTestData>) {
        for RTestData { rs2, result } in args {
            state.apply_inst(&T::new(RD, RS1, rs2));
            assert_eq!(i32::from(state.regfile.read(RD)), result);
        }
    }

    struct IArithTestData {
        imm: i32,
        result: i32,
    }

    /// Tests an I type arithmetic instruction. Assumes that the registers being read
    /// are independent of the registers being written.
    fn test_i_type_arith<T: ITypeArith>(state: &mut ProgramState, args: Vec<IArithTestData>) {
        for IArithTestData { imm, result } in args {
            state.apply_inst(&T::new(RD, RS1, DataWord::from(imm)));
            assert_eq!(i32::from(state.regfile.read(RD)), result);
        }
    }

    #[test]
    fn test_to_binary() {
        // add s0, s1, s2
        const ADD_HEX: u32 = 0x0124_8433;
        assert_eq!(Add::new(S0, S1, S2).to_machine_code(), ADD_HEX);
        // addi t1, t2, -10
        const ADDI_HEX: u32 = 0xFF63_8313;
        assert_eq!(
            Addi::new(T1, T2, DataWord::from(-10)).to_machine_code(),
            ADDI_HEX
        );
        // auipc s1, 10
        const AUIPC_HEX: u32 = 0x0000_A497;
        assert_eq!(
            Auipc::new(S1, DataWord::from(10)).to_machine_code(),
            AUIPC_HEX
        );
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
    fn test_u_type() {
        let mut state = get_init_state();
        state.pc = 0x10FC_0000;
        state.apply_inst(&Auipc::new(RD, DataWord::from(0x10)));
        assert_eq!(
            u32::from(state.regfile.read(RD)),
            0x10FC_0000 + (0x10 << 12)
        );
    }
}

#[allow(dead_code)]
pub enum Instruction {
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
