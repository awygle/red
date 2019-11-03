use crate::ines::*;

pub trait Mapper {
    fn get_byte(&self, addr: u16) -> u8;
    fn get_byte_ppu(&self, addr: u16) -> u8;
    fn set_byte_ppu(&mut self, addr: u16, value: u8);
}

#[derive(Debug)]
pub struct NROM {
    prg_rom_upper: Vec<u8>,
    prg_rom_lower: Vec<u8>,
    chr_rom: Vec<u8>,
    chr_ram: Vec<u8>,
    palette_ram: [u8; 32],
}

impl NROM {
}

pub fn create_mapper(rom: NesRom) -> NROM {
    assert!(rom.mapper_number() == 0);
    
    let mut chr_ram = Vec::with_capacity(2*1024);
    chr_ram.resize_with(2*1024, || 0);
    
    if rom.prg_rom_size_bytes() == 32 * 1024 {
        let prg_rom = rom.prg_rom();
        assert!(prg_rom.len() == rom.prg_rom_size_bytes());
        
        NROM {
            prg_rom_upper: prg_rom[16*1024..].to_vec(),
            prg_rom_lower: prg_rom[..16*1024].to_vec(),
            chr_rom: rom.chr_rom().to_vec(),
            chr_ram,
            palette_ram: [0; 32],
        }
    }
    else if rom.prg_rom_size_bytes() == 16 * 1024 {
        let prg_rom = rom.prg_rom();
        assert!(prg_rom.len() == rom.prg_rom_size_bytes());
        
        NROM {
            prg_rom_upper: prg_rom[..16*1024].to_vec(),
            prg_rom_lower: prg_rom[..16*1024].to_vec(),
            chr_rom: rom.chr_rom().to_vec(),
            chr_ram,
            palette_ram: [0; 32],
        }
    }
    else {
        panic!("invalid PRG ROM size {} for NROM mapper (mapper 0)", rom.prg_rom_size_bytes());
    }
}

impl Mapper for NROM {
    fn get_byte(&self, addr: u16) -> u8 {
        assert!(addr >= 0x4020);
        //println!("CPU memory access at {:#X}", addr);
        
        if addr < 0x8000 {
            // TODO RAM
            println!("Accessing invalid address {:#X}", addr);
            panic!("tried to access nonexistent RAM");
        }
        else if addr < 0xC000 {
            self.prg_rom_lower[addr as usize - 0x8000]
        }
        else {
            self.prg_rom_upper[addr as usize - 0xC000]
        }
    }
    
    fn get_byte_ppu(&self, addr: u16) -> u8 {
        if addr < 0x2000u16 {
            self.chr_rom[addr as usize]
        }
        else if addr >=0x3F00u16 && addr < 0x4000u16 {
            self.palette_ram[(addr & 0x1Fu16) as usize]
        }
        else {
            if addr >= 0x3000u16 {
                println!("Accessing invalid address {:#X}", addr);
            }
            assert!(addr < 0x3000u16);
            self.chr_ram[addr as usize - 0x2000]
        }
    }
    
    fn set_byte_ppu(&mut self, addr: u16, value: u8) {
        //println!("Writing {:#X} to address {:#X}", value, addr);
        assert!(addr >= 0x2000u16 && addr < 0x4000u16);
        if addr >= 0x3F00u16 {
            let effective_addr = addr & 0x1F;
            self.palette_ram[effective_addr as usize] = value;
        }
        else {
            assert!(addr < 0x2400);
            let effective_addr = addr & 0x3FF;
            self.chr_ram[effective_addr as usize] = value;
        }
    }
}

