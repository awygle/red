use std::fs::read;
use std::convert::TryInto;

#[derive(Debug)]
pub struct NesRom {
    header: INesHeader,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl NesRom {
    pub fn from_file(path: &str) -> std::io::Result<NesRom>  {
        let mut contents = read(path)?;
        
        NesRom::from_bytes(&contents)
    }
    
    pub fn from_bytes(contents: &[u8]) -> std::io::Result<NesRom>  {
        let mut ptr = 0;
        let header = INesHeader::from_bytes(&contents[..16]);
        ptr += 16;
        
        let prg_rom :Vec<u8> = contents[ptr..ptr+header.prg_rom_size_bytes()].to_vec();
        ptr += header.prg_rom_size_bytes();
        
        let chr_rom :Vec<u8> = contents[ptr..ptr+header.chr_rom_size_bytes()].to_vec();
        ptr += header.chr_rom_size_bytes();
        
        Ok(NesRom {
            header,
            prg_rom,
            chr_rom,
        })
    }
    
    pub fn prg_rom(&self) -> &[u8] {
        &self.prg_rom
    }
    
    pub fn chr_rom(&self) -> &[u8] {
        &self.chr_rom
    }
    
    pub fn mapper_number(&self) -> u8 {
        self.header.mapper_number()
    }
    
    pub fn prg_rom_size_bytes(&self) -> usize {
        self.header.prg_rom_size_bytes()
    }
}

#[derive(Debug)]
struct INesHeader {
    magic: [u8; 4],
    prg_size: u8,
    chr_size: u8,
    flags_6: u8,
    flags_7: u8,
    flags_8: u8,
    flags_9: u8,
    flags_10: u8,
    zeroes: [u8; 5],
}

impl INesHeader {
    fn from_bytes(input: &[u8]) -> INesHeader {
        assert!(input.len() >= 16);
        
        let result = INesHeader {
            magic: input[..4].try_into().unwrap(),
            prg_size: input[4],
            chr_size: input[5],
            flags_6: input[6],
            flags_7: input[7],
            flags_8: input[8],
            flags_9: input[9],
            flags_10: input[10],
            zeroes: input[11..16].try_into().unwrap(),
        };
        
        assert!(result.magic == [0x4E, 0x45, 0x53, 0x1A]);
        assert!(!result.has_trainer());
        result
    }
    
    fn prg_rom_size_bytes(&self) -> usize {
        self.prg_size as usize * 16*1024
    }
    
    fn chr_rom_size_bytes(&self) -> usize {
        self.prg_size as usize * 8*1024
    }
    
    fn mirrored_horizontal(&self) -> bool {
        (self.flags_6 & 1) == 0
    }
    
    fn mirrored_vertical(&self) -> bool {
        (self.flags_6 & 1) == 1
    }
    
    fn has_persistent_memory(&self) -> bool {
        (self.flags_6 & 2) == 1
    }
    
    fn has_trainer(&self) -> bool {
        (self.flags_6 & 4) == 1
    }
    
    fn four_screen_vram(&self) -> bool {
        (self.flags_6 & 8) == 1
    }
    
    fn vs_unisystem(&self) -> bool {
        (self.flags_7 & 1) == 1
    }
    
    fn playchoice_ten(&self) -> bool {
        (self.flags_7 & 2) == 1
    }
    
    fn is_ines_2(&self) -> bool {
        (self.flags_7 & 0xC) == 0xC
    }
    
    fn mapper_number(&self) -> u8 {
        if self.zeroes.iter().filter(|&&x| x != 0u8).count() > 1 {
            (self.flags_6 >> 4)
        }
        else {
            (self.flags_6 >> 4) | (self.flags_7 & 0xF0)
        }
    }
    
    fn prg_ram_size_bytes(&self) -> usize {
        if self.flags_8 == 0 {
            8*1024
        }
        else {
            self.flags_8 as usize * 8 * 1024
        }
    }
    
    // TODO support iNES 2.0 format
    fn ntsc(&self) -> bool {
        (self.flags_9 & 1) == 0
    }
}

