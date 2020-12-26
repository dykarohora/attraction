pub struct Rom {
    memory: Vec<u8>,
}

impl Rom {
    pub fn new(data: Vec<u8>) -> Rom {
        Rom {
            memory: data
        }
    }
}