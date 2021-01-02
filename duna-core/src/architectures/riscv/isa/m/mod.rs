//! Instructions from the base instruction set and M (multiplication/division) extension.
//!
//! All these instructions follow R-type encodings from the base ISA, and in fact share the same
//! opcodes.
use crate::{
    architectures::riscv::{
        instruction::*,
        isa::i::{f3, f7},
    },
    data_structures::*,
};
use num_traits::ops::checked::{CheckedDiv, CheckedRem};

const R_OPCODE: BitStr32 = BitStr32::new(0b011_0011, 7);
const R_W_OPCODE: BitStr32 = BitStr32::new(0b011_1011, 7);

pub struct Mul;
impl<S: AtLeast32b> RType<S> for Mul {
    fn name() -> &'static str {
        "mul"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        (rs1_val.as_unsigned() * rs2_val.as_unsigned()).into()
    }
}

pub struct Mulw;
impl RType<W64b> for Mulw {
    fn name() -> &'static str {
        "mulw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        DataDword::sign_ext_from_lword(
            (rs1_val.lower_lword().as_unsigned() * rs2_val.lower_lword().as_unsigned()).into(),
        )
    }
}

// /// Performs signed * signed multiplication and returns upper bits of the product
// pub struct Mulh;

// /// Performs unsigned * unsigned multiplication and returns upper bits of the product
// pub struct Mulhu;

// /// Performs signed * unsigned multiplication and returns upper bits of the product
// pub struct Mulhsu;

pub struct Div;
impl<S: AtLeast32b> RType<S> for Div {
    fn name() -> &'static str {
        "div"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b100),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        if rs2_val.is_zero() {
            // According to spec, division by 0 has all bits set in rd rather than a trap
            RegValue::<S>::from(-1i64)
        } else {
            let div = rs1_val
                .as_signed()
                .raw()
                .checked_div(&rs2_val.as_signed().raw());
            // Already checked for div by 0, so this must be overflow
            if let Some(n) = div {
                RegValue::<S>::from_signed(n)
            } else {
                // Per spec, on overflow the most negative number is left in the register
                // Overflow can only occur when rs1_val is said number and rs2_val is -1
                rs1_val
            }
        }
    }
}

pub struct Divw;
impl RType<W64b> for Divw {
    fn name() -> &'static str {
        "divw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b100),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        if rs2_val.is_zero() {
            // According to spec, division by 0 has all bits set in rd rather than a trap
            DataDword::from(-1i64)
        } else {
            let div = rs1_val
                .lower_lword()
                .as_signed()
                .raw()
                .checked_div(rs2_val.lower_lword().as_signed().raw());
            // Already checked for div by 0, so this must be overflow
            if let Some(n) = div {
                DataDword::sign_ext_from_lword(DataLword::from_signed(n))
            } else {
                // Per spec, on overflow the most negative number is left in the register
                // Overflow can only occur when rs1_val is said number and rs2_val is -1
                rs1_val
            }
        }
    }
}

pub struct Divu;
impl<S: AtLeast32b> RType<S> for Divu {
    fn name() -> &'static str {
        "divu"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b101),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let div = rs1_val
            .as_unsigned()
            .raw()
            .checked_div(&rs2_val.as_unsigned().raw());
        if let Some(n) = div {
            RegValue::<S>::from_unsigned(n)
        } else {
            // Overflow is impossible for unsigned, so this must be div by 0
            RegValue::<S>::from(-1i64)
        }
    }
}

pub struct Divuw;
impl RType<W64b> for Divuw {
    fn name() -> &'static str {
        "divuw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b101),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        let div = rs1_val
            .lower_lword()
            .as_unsigned()
            .raw()
            .checked_div(rs2_val.lower_lword().as_unsigned().raw());
        if let Some(n) = div {
            DataDword::sign_ext_from_lword(DataLword::from_unsigned(n))
        } else {
            // Overflow is impossible for unsigned, so this must be div by 0
            DataDword::from(-1i64)
        }
    }
}

pub struct Rem;
impl<S: AtLeast32b> RType<S> for Rem {
    fn name() -> &'static str {
        "rem"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b110),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        if rs2_val.is_zero() {
            // According to spec, division by 0 in rem propagates first argument
            rs1_val
        } else {
            let rem = rs1_val
                .as_signed()
                .raw()
                .checked_rem(&rs2_val.as_signed().raw());
            // Already checked for div by 0, so this must be overflow
            if let Some(n) = rem {
                RegValue::<S>::from_signed(n)
            } else {
                // Per spec, on overflow 0 is left in the register
                RegValue::<S>::zero()
            }
        }
    }
}

pub struct Remw;
impl RType<W64b> for Remw {
    fn name() -> &'static str {
        "remw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b110),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        if rs2_val.is_zero() {
            // According to spec, division by 0 in rem propagates first argument
            // and sign-extends the 32-bit value
            DataDword::sign_ext_from_lword(rs1_val.lower_lword())
        } else {
            let rem = rs1_val
                .lower_lword()
                .as_signed()
                .raw()
                .checked_rem(rs2_val.lower_lword().as_signed().raw());
            // Already checked for div by 0, so this must be overflow
            if let Some(n) = rem {
                DataDword::sign_ext_from_lword(DataLword::from_signed(n))
            } else {
                // Per spec, on overflow 0 is left in the register
                DataDword::zero()
            }
        }
    }
}

pub struct Remu;
impl<S: AtLeast32b> RType<S> for Remu {
    fn name() -> &'static str {
        "remu"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b110),
            opcode: R_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<S>, rs2_val: RegValue<S>) -> RegValue<S> {
        let rem = rs1_val
            .as_unsigned()
            .raw()
            .checked_rem(&rs2_val.as_unsigned().raw());
        if let Some(n) = rem {
            RegValue::<S>::from_unsigned(n)
        } else {
            // Overflow is impossible for unsigned, so this must be div by 0
            RegValue::<S>::zero()
        }
    }
}

pub struct Remuw;
impl RType<W64b> for Remuw {
    fn name() -> &'static str {
        "remuw"
    }

    fn inst_fields() -> RInstFields {
        RInstFields {
            funct7: f7(1),
            funct3: f3(0b111),
            opcode: R_W_OPCODE,
        }
    }

    fn eval(rs1_val: RegValue<W64b>, rs2_val: RegValue<W64b>) -> RegValue<W64b> {
        let rem = rs1_val
            .lower_lword()
            .as_unsigned()
            .raw()
            .checked_rem(rs2_val.lower_lword().as_unsigned().raw());
        if let Some(n) = rem {
            DataDword::sign_ext_from_lword(DataLword::from_unsigned(n))
        } else {
            // Overflow is impossible for unsigned, so this must be div by 0
            DataDword::zero()
        }
    }
}
