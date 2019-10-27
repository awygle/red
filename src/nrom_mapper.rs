use crate::ines::*;

pub trait Mapper {
    fn get_byte(&self, addr: u16) -> u8;
}

#[derive(Debug)]
pub struct NROM {
    prg_rom_upper: Vec<u8>,
    prg_rom_lower: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl NROM {
}

pub fn create_mapper(rom: NesRom) -> NROM {
    assert!(rom.mapper_number() == 0);
    
    if rom.prg_rom_size_bytes() == 32 * 1024 {
        let prg_rom = rom.prg_rom();
        assert!(prg_rom.len() == rom.prg_rom_size_bytes());
        
        NROM {
            prg_rom_upper: prg_rom[16*1024..].to_vec(),
            prg_rom_lower: prg_rom[..16*1024].to_vec(),
            chr_rom: rom.chr_rom().to_vec(),
        }
    }
    else if rom.prg_rom_size_bytes() == 16 * 1024 {
        let prg_rom = rom.prg_rom();
        assert!(prg_rom.len() == rom.prg_rom_size_bytes());
        
        NROM {
            prg_rom_upper: prg_rom[..16*1024].to_vec(),
            prg_rom_lower: prg_rom[..16*1024].to_vec(),
            chr_rom: rom.chr_rom().to_vec(),
        }
    }
    else {
        panic!("invalid PRG ROM size {} for NROM mapper (mapper 0)", rom.prg_rom_size_bytes());
    }
}

impl Mapper for NROM {
    fn get_byte(&self, addr: u16) -> u8 {
        assert!(addr >= 0x4020);
        
        if addr < 0x8000 {
            // TODO RAM
            0
        }
        else if addr < 0xC000 {
            self.prg_rom_lower[addr as usize - 0x8000]
        }
        else {
            self.prg_rom_upper[addr as usize - 0xC000]
        }
    }
    
    fn get_byte_ppu(&self, addr: u16) -> u8 {
        unimplemented!();
    }
}

