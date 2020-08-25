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
    pub kind: PtKind,
}

impl Default for MemConfig {
    fn default() -> Self {
        MemConfig {
            // 4 KiB page size, 4 MiB physical memory
            kind: PtKind::FifoLinearPaged {
                phys_pn_bits: 10,
                pg_ofs_bits: 12,
            },
        }
    }
}

impl MemConfig {
    pub fn build_mem<T: ByteAddress>(&self) -> Box<dyn PageTable<T>> {
        use crate::program_state::*;
        let kind = self.kind;
        match kind {
            PtKind::AllMapped => Box::new(AllMappedPt::<T>::new()),
            PtKind::FifoLinearPaged {
                phys_pn_bits,
                pg_ofs_bits,
            } => Box::new(FifoLinearPt::<T>::new(phys_pn_bits, pg_ofs_bits)),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PtKind {
    AllMapped,
    FifoLinearPaged {
        phys_pn_bits: usize,
        pg_ofs_bits: usize,
    },
}
