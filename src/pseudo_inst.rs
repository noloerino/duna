use crate::instruction::{ConcreteInst, ITypeArith, UType};
use crate::isa::*;
use crate::program_state::IRegister::*;
use crate::program_state::*;

/// Represents pseudo-instructions that take a register and an immediate as arguments.
pub trait RegImm {
    fn expand(reg: IRegister, data: DataWord) -> Vec<ConcreteInst>;
}

pub struct Li;
impl RegImm for Li {
    fn expand(reg: IRegister, data: DataWord) -> Vec<ConcreteInst> {
        let imm = data.to_bit_str(32);
        let mut upper = imm.slice(32, 12);
        let lower = imm.slice(11, 0);
        // If upper is not all 1s (which means the number isn't just a negative addi) and the MSB
        // of lower is high, then we need to add 1 to the upper immediate for sign extension reasons
        // The former case (all 1s) is covered by adding 1 due to overflow being truncated
        if lower.index(11).as_u32() > 0 {
            // This will never overflow because upper only has 20 bits of data
            upper = BitStr32::new(upper.as_u32() + 1, 20);
        }
        let no_lui = upper.is_zero();
        let rs1 = if no_lui { ZERO } else { reg };
        let addi = Addi::new(reg, rs1, DataWord::from(lower.as_u32()));
        if upper.is_zero() {
            vec![addi]
        } else {
            vec![Lui::new(reg, DataWord::from(upper.as_u32())), addi]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_li() {
        // Tests various expansions of li
        // First one requires messing with sign
        let dead_beef = Li::expand(A0, DataWord::from(0xDEAD_BEEFu32));
        assert_eq!(dead_beef.len(), 2);
        assert_eq!(dead_beef[0], Lui::new(A0, DataWord::from(0xD_EADC)));
        assert_eq!(dead_beef[1], Addi::new(A0, A0, DataWord::from(-273)));
        // Edge case for sign
        let eef = Li::expand(A0, DataWord::from(0xEEF));
        assert_eq!(eef.len(), 2);
        assert_eq!(eef[0], Lui::new(A0, DataWord::from(1)));
        assert_eq!(eef[1], Addi::new(A0, A0, DataWord::from(-273)));
        // Negative, but lower is just -1
        let low_neg_1 = Li::expand(A0, DataWord::from(0xFFAB_FFFFu32));
        assert_eq!(low_neg_1.len(), 2);
        assert_eq!(low_neg_1[0], Lui::new(A0, DataWord::from(0xF_FAC0)));
        assert_eq!(low_neg_1[1], Addi::new(A0, A0, DataWord::from(-1)));
        // Another such case with more complicated lower
        let abcd_abcd = Li::expand(A0, DataWord::from(0xABCD_ABCDu32));
        assert_eq!(abcd_abcd.len(), 2);
        assert_eq!(abcd_abcd[0], Lui::new(A0, DataWord::from(0xABCDB)));
        assert_eq!(abcd_abcd[1], Addi::new(A0, A0, DataWord::from(-1075)));
        // Negative, but just expands to addi
        let num = Li::expand(A0, DataWord::from(-273));
        assert_eq!(num.len(), 1);
        assert_eq!(num[0], Addi::new(A0, ZERO, DataWord::from(-273)));
    }
}
