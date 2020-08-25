use crate::program_state::*;

/// Options for the assembler.
#[derive(Debug)]
pub struct AsmConfig {
    /// Parameters for the machine being emulated.
    pub machine: MachineConfig,
}

impl Default for AsmConfig {
    fn default() -> Self {
        AsmConfig {
            machine: Default::default(),
        }
    }
}

/// Configuration for the machine being emulated.
#[derive(Debug)]
pub struct MachineConfig {
    pub mem_config: MemConfig,
}

impl Default for MachineConfig {
    fn default() -> Self {
        MachineConfig {
            mem_config: Default::default(),
        }
    }
}

/// Configures a memory device.
/// TODO add options for alignment and default value
#[derive(Debug)]
pub struct MemConfig {
    pub kind: MemKind,
}

impl Default for MemConfig {
    fn default() -> Self {
        MemConfig {
            // 4 KiB page size, 4 MiB physical memory
            kind: MemKind::LinearPaged {
                phys_addr_len: 22,
                page_offs_len: 12,
            },
        }
    }
}

impl MemConfig {
    pub fn build_mem<T: ByteAddress>(&self) -> Box<dyn PageTable<T>> {
        use crate::program_state::*;
        let kind = self.kind;
        match kind {
            MemKind::Simple => Box::new(AllMappedPt::<T>::new()),
            _ => unimplemented!()
            // MemKind::LinearPaged {
            //     phys_addr_len,
            //     page_offs_len,
            // } => Box::new(LinearPagedMemory::<T>::new(phys_addr_len, page_offs_len)),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MemKind {
    Simple,
    LinearPaged {
        phys_addr_len: usize,
        page_offs_len: usize,
    },
}
