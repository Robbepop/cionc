#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(x: usize) -> NodeId {
        use std::u32;
        assert!(x < (u32::MAX as usize));
        NodeId(x as u32)
    }

    pub fn from_u32(x: u32) -> NodeId {
        NodeId(x)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}
