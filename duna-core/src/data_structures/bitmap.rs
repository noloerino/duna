/// A simple implementation of an integer-backed bitmap, for use in representing free lists for
/// disk blocks or memory sectors.
///
/// The map itself is backed by a vector of u64s that are preallocated to 0.
pub struct Bitmap {
    bit_cnt: usize,
    bits: Vec<u64>,
}

impl Bitmap {
    /// Constructs a new bitmap with bit_cnt entries.
    pub fn new(bit_cnt: usize) -> Bitmap {
        let mut b = Bitmap {
            bit_cnt,
            bits: vec![],
        };
        b.clear();
        b
    }

    pub fn clear(&mut self) {
        // if we need 0 bits then we allocate 0 ints, but if we need 1 bit we need to allocate
        // 1 int even though 1 / 64 = 0
        let round_up = self.bit_cnt % 64 != 0;
        self.bits = vec![0; self.bit_cnt / 64 + (if round_up { 1 } else { 0 })];
    }

    fn bounds_check(&self, idx: usize) {
        if idx >= self.bit_cnt {
            panic!(
                "Attempted to access index {} for bitmap of size {}",
                idx, self.bit_cnt
            )
        }
    }

    /// Reads an entry in the bitmap. Panics if the index is out of bounds.
    pub fn read(&self, idx: usize) -> bool {
        self.bounds_check(idx);
        let n = self.bits[idx / 64];
        let bit_idx = idx % 64;
        ((n >> bit_idx) & 0b1) != 0
    }

    /// Flips an entry in the bitmap. Panics if the index is out of bounds.
    pub fn flip(&mut self, idx: usize) {
        self.bounds_check(idx);
        // to flip one bit, XOR the entry with a mask of all 0s
        // except at the desired index
        let bit_idx = idx % 64;
        let mask = 1 << bit_idx;
        let k = idx / 64;
        self.bits[k] ^= mask;
    }

    /// Returns the index of the lowest 0 bit, or None if all entries are 1.
    pub fn get_lowest_zero(&self) -> Option<usize> {
        // more efficient way would be to loop over vec entries to minimize lookups, but we don't
        // care that much about performance
        for i in 0..self.bit_cnt {
            if !self.read(i) {
                return Some(i);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Bitmap;

    /// Tests a bitmap backed by only a single integer.
    #[test]
    fn test_bitmap_small() {
        let mut bm = Bitmap::new(64);
        assert_eq!(bm.get_lowest_zero().unwrap(), 0);
        bm.flip(0);
        bm.flip(31);
        assert_eq!(bm.get_lowest_zero().unwrap(), 1);
        // extra checks ensure no off-by-one errors
        assert!(bm.read(0));
        assert!(!bm.read(1));
        assert!(!bm.read(30));
        assert!(bm.read(31));
        assert!(!bm.read(32));
        bm.flip(0);
        assert_eq!(bm.get_lowest_zero().unwrap(), 0);
        assert!(!bm.read(0));
        assert!(!bm.read(1));
    }

    /// Tests a bitmap backed by several integers.
    #[test]
    fn test_bitmap_several() {
        let mut bm = Bitmap::new(256);
        for i in 0..10 {
            bm.flip(i);
        }
        bm.flip(31);
        bm.flip(142);
        bm.flip(255);
        assert_eq!(bm.get_lowest_zero().unwrap(), 10);
        for i in 0..10 {
            assert!(bm.read(i));
        }
        assert!(bm.read(31));
        assert!(bm.read(142));
        assert!(bm.read(255));
        bm.flip(142);
        assert!(!bm.read(142));
        // check that holes work
        bm.flip(7);
        assert_eq!(bm.get_lowest_zero().unwrap(), 7);
    }

    #[test]
    #[should_panic]
    fn test_bitmap_oob() {
        Bitmap::new(128).read(128);
    }

    #[test]
    fn test_bitmap_full() {
        let mut bm = Bitmap::new(8);
        for i in 0..8 {
            bm.flip(i);
        }
        assert_eq!(bm.get_lowest_zero(), None);
        bm.flip(2);
        assert_eq!(bm.get_lowest_zero().unwrap(), 2);
    }
}
