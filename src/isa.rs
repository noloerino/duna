use crate::instruction::*;

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
    Xori
}

impl RType for Instruction::Add {}
impl IType for Instruction::Addi {}
impl RType for Instruction::And {}
impl IType for Instruction::Andi {}
impl UType for Instruction::Auipc {}
impl BType for Instruction::Beq {}
impl BType for Instruction::Bge {}
impl BType for Instruction::Bgeu {}
impl BType for Instruction::Blt {}
impl BType for Instruction::Bltu {}
impl BType for Instruction::Bne {}
impl EnvironInst for Instruction::Ebreak {}
impl EnvironInst for Instruction::Ecall {}
impl JType for Instruction::Jal {}
impl IType for Instruction::Jalr {}
impl IType for Instruction::Lb {}
impl IType for Instruction::Lbu {}
impl IType for Instruction::Ld {}
impl IType for Instruction::Lh {}
impl IType for Instruction::Lhu {}
impl UType for Instruction::Lui {}
impl IType for Instruction::Lw {}
impl IType for Instruction::Lwu {}
impl RType for Instruction::Or {}
impl IType for Instruction::Ori {}
impl SType for Instruction::Sb {}
impl SType for Instruction::Sd {}
impl SType for Instruction::Sh {}
impl RType for Instruction::Sll {}
impl IType for Instruction::Slli {}
impl RType for Instruction::Slt {}
impl IType for Instruction::Slti {}
impl IType for Instruction::Sltiu {}
impl RType for Instruction::Sltu {}
impl RType for Instruction::Sra {}
impl IType for Instruction::Srai {}
impl RType for Instruction::Srl {}
impl IType for Instruction::Srli {}
impl RType for Instruction::Sub {}
impl SType for Instruction::Sw {}
impl RType for Instruction::Xor {}
impl IType for Instruction::Xori {}
