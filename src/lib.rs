// #![allow(dead_code)]
#[macro_use]
extern crate lazy_static;
pub mod instruction;
pub mod isa;
mod lexer;
mod parser;
pub mod program_state;
pub mod pseudo_inst;
