extern crate red;

use std::fs::read;
use red::ines::*;
use red::nrom_mapper::*;
use red::cpu;

#[test]
fn nestest() {
    let path = "./nestest.nes";
    let mut contents = read(path).unwrap();
    contents[16+16*1024-3] = 0xC0;
    contents[16+16*1024-4] = 0x00;
    
    let rom = NesRom::from_bytes(&contents).unwrap();
    let mapper = create_mapper(rom);
    let mut cpu = cpu::CPU::with_mapper(Box::new(mapper));
    
    println!("reset vector: {:#X}", cpu.get_pc());
    cpu.execute();
    cpu.execute();
    cpu.execute();
    dbg!(cpu);
}

