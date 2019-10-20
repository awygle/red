use crate::nrom_mapper::*;
use std::collections::VecDeque;

struct Registers {
    pc: u16,
    s: u8,
    p: u8,
    a: u8,
    x: u8,
    y: u8,
}

pub struct CPU {
    regs: Registers,
    mapper: Box<Mapper>,
    idl: u8,
    queue: VecDeque<Microcode>,
    instruction_rom: Vec<Vec<MicroFn>>,
}

#[derive(Copy, Clone)]
struct Microcode {
    f: fn(&mut CPU),
}

#[derive(Debug, Copy, Clone)]
enum MicroFn {
    FetchAndDecode,
    FetchToIdl,
    FetchAndCopy,
}

fn fetch_and_decode(cpu: &mut CPU) {
    println!("in fetch_and_decode");
    let op = cpu.fetch_byte(cpu.get_pc());
    let uops = &cpu.instruction_rom[op as usize]; // Vec<MicroFn>
    for uop in uops.into_iter().rev() {
        println!("adding uop {:?} to queue", *uop);
        cpu.queue.push_front(Microcode {
            f: (*uop).into()
        });
    }
}

fn fetch_to_idl(cpu: &mut CPU) {
    cpu.idl = cpu.fetch_byte(cpu.get_pc());
}

fn fetch_and_copy(cpu: &mut CPU) {
    cpu.regs.pc = (cpu.idl as u16) | ((cpu.fetch_byte(cpu.get_pc()) as u16) << 8);
}

impl From<MicroFn> for fn(&mut CPU) {
    fn from(x: MicroFn) -> fn(&mut CPU) {
        match x {
            MicroFn::FetchAndDecode => fetch_and_decode,
            MicroFn::FetchToIdl => fetch_to_idl,
            MicroFn::FetchAndCopy => fetch_and_copy,
        }
    }
}

impl Microcode {
    pub fn fetch(addr: u16) -> Microcode {
        Microcode {
            f: fetch_to_idl,
        }
    }
}

const JMP_ABS :u8 = 0x4C;

impl CPU {
    fn instructions() -> Vec<Vec<MicroFn>> {
        let mut result = Vec::new();
        result.resize_with(256, || vec![]);
        
        result[JMP_ABS as usize] = vec![ 
            // fetch low byte address, increment PC
            MicroFn::FetchToIdl,
            // copy low address byte to PCL, fetch high address byte to PCH
            MicroFn::FetchAndCopy,
        ];
        
        result
    }
    
    pub fn with_mapper(mapper: Box<Mapper>) -> CPU {
        let reset_vector = ((mapper.get_byte(0xFFFD) as u16) << 8) | (mapper.get_byte(0xFFFC) as u16);
        let mut queue = VecDeque::new();
        queue.push_front(Microcode {
            f: MicroFn::FetchAndDecode.into(),
        });
        
        CPU {
            regs: Registers {
                pc: reset_vector,
                s: 0u8,
                p: 0x30u8,
                a: 0u8,
                x: 0u8,
                y: 0u8,
            },
            mapper: mapper,
            idl: 0,
            queue,
            instruction_rom: CPU::instructions(),
        }
    }
    
    pub fn get_pc(&self) -> u16 {
        self.regs.pc
    }
    
    pub fn get_byte(&self, addr: u16) -> u8 {
        self.mapper.get_byte(addr)
    }
    
    pub fn fetch_byte(&mut self, addr: u16) -> u8 {
        let result = self.get_byte(addr);
        self.regs.pc += 1;
        result
    }
    
    pub fn execute(&mut self) {
        let uop = self.queue.pop_front().unwrap();
        
        (uop.f)(self);
    }
}
