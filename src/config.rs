use crate::arch::MachineDataWidth;
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
    pub text_start: usize,
    pub stack_start: usize,
    pub data_start: usize,
}

impl Default for SegmentStarts {
    fn default() -> Self {
        // TODO configure boundaries for 64 vs 32-bit
        SegmentStarts {
            text_start: 0x1000_0000,
            stack_start: 0x7FFF_FFFF0,
            data_start: 0x2000_0000,
        }
    }
}

impl SegmentStarts {
    pub fn text<T: MachineDataWidth>(&self) -> T::ByteAddr {
        <T as MachineDataWidth>::usize_to_usgn(self.text_start).into()
    }

    pub fn data<T: MachineDataWidth>(&self) -> T::ByteAddr {
        <T as MachineDataWidth>::usize_to_usgn(self.data_start).into()
    }

    pub fn stack<T: MachineDataWidth>(&self) -> T::ByteAddr {
        <T as MachineDataWidth>::usize_to_usgn(self.stack_start).into()
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
            segment_starts: Default::default(),
            mem_config: Default::default(),
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
    pub fn build_mem<T: ByteAddress>(&self) -> Box<dyn PageTable<T>> {
        use crate::program_state::*;
        let kind = self.kind;
        match kind {
            PtKind::AllMapped => Box::new(AllMappedPt::<T>::new()),
            PtKind::FifoLinearPaged => {
                Box::new(FifoLinearPt::<T>::new(self.phys_pn_bits, self.pg_ofs_bits))
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PtKind {
    AllMapped,
    FifoLinearPaged,
}
