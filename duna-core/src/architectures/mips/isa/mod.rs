mod r_type;

use crate::data_structures::*;
pub use r_type::*;

fn funct(val: u32) -> BitStr32 {
    BitStr32::new(val, 6)
}
