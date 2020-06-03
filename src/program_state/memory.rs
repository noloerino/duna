use super::datatypes::*;
use std::collections::HashMap;

pub struct Memory<T: MachineDataWidth> {
    store: HashMap<<T::ByteAddr as ByteAddress>::WordAddress, DataWord>,
}

impl<T: MachineDataWidth> Memory<T> {
    pub(in crate::program_state) fn new() -> Memory<T> {
        Memory {
            store: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn set_byte(&mut self, addr: T::ByteAddr, value: DataByte) {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        let word_val = if let Some(&old_val) = self.store.get(&word_addr) {
            old_val
        } else {
            DataWord::zero()
        };
        self.set_word(word_addr, word_val.set_byte(offs, value))
    }

    pub fn get_byte(&self, addr: T::ByteAddr) -> DataByte {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        self.get_word(word_addr).get_byte(offs)
    }

    pub fn set_word(&mut self, addr: <T::ByteAddr as ByteAddress>::WordAddress, value: DataWord) {
        self.store.insert(addr, value);
    }

    pub fn get_word(&self, addr: <T::ByteAddr as ByteAddress>::WordAddress) -> DataWord {
        if let Some(&v) = self.store.get(&addr) {
            v
        } else {
            DataWord::zero()
        }
    }
}
