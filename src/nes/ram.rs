pub struct Ram {
    memory: Vec<u8>
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            memory: vec![0; 2048]
        }
    }
}