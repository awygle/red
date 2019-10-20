use crate::nrom_mapper::*;

struct Registers {
    pc: u16,
    s: u8,
    p: u8,
    a: u8,
    x: u8,
    y: u8,
}

struct CPU {
    regs: Registers,
}

impl CPU {
    fn with_mapper(mapper: impl Mapper) -> CPU {
        let reset_vector = ((mapper.get_byte(0xFFFD) as u16) << 8) | (mapper.get_byte(0xFFFC) as u16);
        CPU {
            pc: reset_vector,
            s: 0u8,
            p: 0x30u8,
            a: 0u8,
            x: 0u8,
            y: 0u8,
        }
    }
}
