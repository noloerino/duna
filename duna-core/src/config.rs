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

/// Configures the start of the text, stack, and data segments.
#[derive(Debug, Copy, Clone)]
pub struct SegmentStarts {
    pub text_start: u64,
    pub stack_start: u64,
    pub data_start: u64,
}

impl Default for SegmentStarts {
    fn default() -> Self {
        // taken from Venus
        SegmentStarts {
            text_start: 0x1000_0000,
            stack_start: 0x7FFF_FFF0,
            data_start: 0x2000_0000,
        }
    }
}

// impl Default for SegmentStarts<W64b> {
//     fn default() -> Self {
//         // taken from the RISCV green card
//         SegmentStarts {
//             text_start: ByteAddr64::from_unsigned(0x1000_0000),
//             stack_start: ByteAddr64::from_unsigned(0x003F_FFFF_FFF0),
//             data_start: ByteAddr64::from_unsigned(0x0040_0000),
//         }
//     }
// }

impl SegmentStarts {
    pub fn text<S: DataWidth>(&self) -> ByteAddrValue<S> {
        ByteAddrValue::<S>::from(self.text_start)
    }

    pub fn data<S: DataWidth>(&self) -> ByteAddrValue<S> {
        ByteAddrValue::<S>::from(self.data_start)
    }

    pub fn stack<S: DataWidth>(&self) -> ByteAddrValue<S> {
        ByteAddrValue::<S>::from(self.stack_start)
    }
}

/// Configuration for the machine being emulated.
#[derive(Debug)]
pub struct MachineConfig {
    pub segment_starts: SegmentStarts,
    pub mem_config: MemConfig,
}

impl Default for MachineConfig {
    fn default() -> Self {
        MachineConfig {
            segment_starts: SegmentStarts::default(),
            mem_config: MemConfig::default(),
        }
    }
}

/// Configures a memory device.
/// TODO add options for alignment and default value
#[derive(Debug)]
pub struct MemConfig {
    pub phys_pn_bits: usize,
    pub pg_ofs_bits: usize,
    pub kind: PtKind,
}

impl Default for MemConfig {
    fn default() -> Self {
        MemConfig {
            phys_pn_bits: 10,
            pg_ofs_bits: 12,
            // 4 KiB page size, 4 MiB physical memory
            kind: PtKind::FifoLinearPaged,
        }
    }
}

impl MemConfig {
    pub fn build_mem<S: DataWidth>(&self) -> Box<dyn PageTable<S>> {
        use crate::program_state::*;
        let kind = self.kind;
        match kind {
            PtKind::AllMapped => Box::new(AllMappedPt::<S>::new()),
            PtKind::FifoLinearPaged => {
                Box::new(FifoLinearPt::<S>::new(self.phys_pn_bits, self.pg_ofs_bits))
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PtKind {
    AllMapped,
    FifoLinearPaged,
}
