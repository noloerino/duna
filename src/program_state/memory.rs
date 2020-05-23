use crate::program_state::datatypes::*;
use std::collections::HashMap;

pub struct Memory {
    store: HashMap<WordAddress, DataWord>,
}

impl Memory {
    pub(in crate::program_state) fn new() -> Memory {
        Memory {
            store: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn set_byte(&mut self, addr: ByteAddress, value: DataByte) {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        let word_val = if let Some(&old_val) = self.store.get(&word_addr) {
            old_val
        } else {
            DataWord::zero()
        };
        self.set_word(word_addr, word_val.set_byte(offs, value))
    }

    pub fn get_byte(&self, addr: ByteAddress) -> DataByte {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        self.get_word(word_addr).get_byte(offs)
    }

    pub fn set_word(&mut self, addr: WordAddress, value: DataWord) {
        self.store.insert(addr, value);
    }

    pub fn get_word(&self, addr: WordAddress) -> DataWord {
        if let Some(&v) = self.store.get(&addr) {
            v
        } else {
            DataWord::zero()
        }
    }
}
