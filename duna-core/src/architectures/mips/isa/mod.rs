mod r_type;

use crate::program_state::*;
pub use r_type::*;

fn funct(val: u32) -> BitStr32 {
    BitStr32::new(val, 6)
}
