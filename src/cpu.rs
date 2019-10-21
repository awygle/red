use crate::nrom_mapper::*;
use std::collections::VecDeque;

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum RegNames {
    PCH = 2,
    PCL = 3,
    S = 4,
    P = 5,
    A = 6,
    X = 7,
    Y = 8,
    IDL = 9,
    ABL = 10,
    ABH = 11,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum Flags {
    C = 0x01,
}

struct Registers {
    pch: u8,
    pcl: u8,
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
    abl: u8,
    abh: u8,
    queue: VecDeque<MicroFn>,
    instruction_rom: Vec<Vec<MicroFn>>,
    ram: [u8; 0x800],
}

#[derive(Debug, Copy, Clone)]
enum MicroFn {
    FetchAndDecode,
    FetchAndCopy,
    BranchSet(Flags),
    BranchClear(Flags),
    ReadTo(RegNames),
    FetchTo(RegNames),
    Write(RegNames),
    Push(RegNames),
    SetFlag(Flags),
    ClearFlag(Flags),
    Nop,
}

fn fetch_and_decode(cpu: &mut CPU, _: u8) {
    let op = cpu.fetch_byte(cpu.get_pc());
    let uops = &cpu.instruction_rom[op as usize]; // Vec<MicroFn>
    assert!(uops.len() > 0, format!("invalid instruction {:#X?}!", op));
    for uop in uops.into_iter().rev() {
        println!("adding uop {:?} to queue", *uop);
        cpu.queue.push_front(*uop);
    }
}

fn fetch_and_copy(cpu: &mut CPU, _: u8) {
    cpu.regs.pch = cpu.fetch_byte(cpu.get_pc());
    cpu.regs.pcl = cpu.idl;
}

impl From<MicroFn> for fn(&mut CPU, u8) {
    fn from(x: MicroFn) -> fn(&mut CPU, u8) {
        match x {
            MicroFn::FetchAndDecode => fetch_and_decode,
            MicroFn::FetchAndCopy => fetch_and_copy,
            MicroFn::BranchSet(flag) => {
                match flag {
                    Flags::C => |cpu, _| {
                        if cpu.regs.p & (Flags::C as u8) > 0 {
                            let tmp_pc = (cpu.regs.pcl as u16) + (cpu.idl as u16);
                            if tmp_pc & 0xFF00 != 0 {
                                cpu.regs.pcl = tmp_pc as u8;
                                cpu.regs.pch += 1;
                                cpu.queue.push_front(MicroFn::FetchAndDecode);
                                cpu.queue.push_front(MicroFn::Nop);
                            }
                            else {
                                cpu.regs.pcl = tmp_pc as u8;
                                cpu.queue.push_front(MicroFn::FetchAndDecode);
                            }
                        }
                        else {
                            fetch_and_decode(cpu, 0);
                        }
                    }
                }
            },
            MicroFn::BranchClear(flag) => {
                match flag {
                    Flags::C => |cpu, _| {
                        if cpu.regs.p & !(Flags::C as u8) == 0 {
                            let tmp_pc = (cpu.regs.pcl as u16) + (cpu.idl as u16);
                            if tmp_pc & 0xFF00 != 0 {
                                cpu.regs.pcl = tmp_pc as u8;
                                cpu.regs.pch += 1;
                                cpu.queue.push_front(MicroFn::FetchAndDecode);
                                cpu.queue.push_front(MicroFn::Nop);
                            }
                            else {
                                cpu.regs.pcl = tmp_pc as u8;
                                cpu.queue.push_front(MicroFn::FetchAndDecode);
                            }
                        }
                        else {
                            fetch_and_decode(cpu, 0);
                        }
                    }
                }
            },
            MicroFn::FetchTo(regname) => {
                let regnum = regname as u8;
                |cpu, regnum| *cpu.get_reg_by_number(regnum) = cpu.fetch_byte(cpu.get_pc())
            },
            MicroFn::ReadTo(regname) => {
                let regnum = regname as u8;
                |cpu, regnum| *cpu.get_reg_by_number(regnum) = cpu.fetch_byte(cpu.get_ab())
            },
            MicroFn::Write(regname) => {
                let regnum = regname as u8;
                |cpu, regnum| {
                    let val: u8 = *cpu.get_reg_by_number(regnum);
                    cpu.set_byte(cpu.get_ab(), val);
                }
            },
            MicroFn::Push(regname) => {
                let regnum = regname as u8;
                |cpu, regnum| {
                    let val: u8 = *cpu.get_reg_by_number(regnum);
                    cpu.set_byte(cpu.regs.s as u16 + 0x100, val);
                    cpu.regs.s.wrapping_sub(1);
                }
            },
            MicroFn::SetFlag(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum| cpu.regs.p |= flagnum
            },
            MicroFn::ClearFlag(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum| cpu.regs.p &= !flagnum
            },
            MicroFn::Nop => |_, _| {},
        }
    }
}

const JMP_ABS :u8 = 0x4C;
const LDX_IMM :u8 = 0xA2;
const LDX_ABS :u8 = 0xAE;
const STX_ZPG :u8 = 0x86;
const JSR :u8 = 0x20;
const NOP :u8 = 0xEA;
const SEC :u8 = 0x38;
const CLC :u8 = 0x18;
const BCS :u8 = 0xB0;
const BCC :u8 = 0x90;

impl CPU {
    fn instructions() -> Vec<Vec<MicroFn>> {
        let mut result = Vec::new();
        result.resize_with(256, || vec![]);
        
        result[JMP_ABS as usize] = vec![ 
            // fetch low byte address, increment PC
            MicroFn::FetchTo(RegNames::IDL),
            // copy low address byte to PCL, fetch high address byte to PCH
            MicroFn::FetchAndCopy,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_IMM as usize] = vec![ 
            MicroFn::FetchTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[STX_ZPG as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Write(RegNames::X),
            MicroFn::FetchAndDecode,
        ];
        
        result[JSR as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::Push(RegNames::PCH),
            MicroFn::Push(RegNames::PCL),
            MicroFn::FetchAndCopy,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[SEC as usize] = vec![
            MicroFn::SetFlag(Flags::C),
            MicroFn::FetchAndDecode,
        ];
        
        result[CLC as usize] = vec![
            MicroFn::ClearFlag(Flags::C),
            MicroFn::FetchAndDecode,
        ];
        
        result[BCS as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchSet(Flags::C),
        ];
        
        result[BCC as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchClear(Flags::C),
        ];
        
        result
    }
    
    pub fn get_ab(&self) -> u16 {
        (self.abl as u16) | ((self.abh as u16) << 8)
    }
    
    pub fn with_mapper(mapper: Box<Mapper>) -> CPU {
        let reset_vector = ((mapper.get_byte(0xFFFD) as u16) << 8) | (mapper.get_byte(0xFFFC) as u16);
        let mut queue = VecDeque::new();
        queue.push_front(MicroFn::FetchAndDecode);
        
        CPU {
            regs: Registers {
                pch: (reset_vector >> 8) as u8,
                pcl: reset_vector as u8,
                s: 0xFDu8,
                p: 0x34u8,
                a: 0u8,
                x: 0u8,
                y: 0u8,
            },
            mapper: mapper,
            idl: 0,
            queue,
            instruction_rom: CPU::instructions(),
            abl: 0,
            abh: 0,
            ram: [0; 0x800],
        }
    }
    
    pub fn get_pc(&self) -> u16 {
        ((self.regs.pch as u16) << 8) | (self.regs.pcl as u16)
    }
    
    pub fn get_byte(&self, addr: u16) -> u8 {
        self.mapper.get_byte(addr)
    }
    
    pub fn fetch_byte(&mut self, addr: u16) -> u8 {
        let result = self.get_byte(addr);
        if self.regs.pcl == 0xFF {
            self.regs.pch += 1;
            self.regs.pcl = 0;
        }
        else {
            self.regs.pcl += 1;
        }
        result
    }
    
    pub fn set_byte(&mut self, addr: u16, value: u8) {
        let effective_addr;
        if addr < 0x2000u16 {
            effective_addr = addr & 0x7FF;
        }
        else {
            panic!("address not implemented");
        }
        self.ram[effective_addr as usize] = value;
    }
    
    pub fn get_reg_by_number(&mut self, regnum: u8) -> &mut u8 {
        match regnum {
            2 => &mut self.regs.pch,
            3 => &mut self.regs.pcl,
            4 => &mut self.regs.s,
            5 => &mut self.regs.p,
            6 => &mut self.regs.a,
            7 => &mut self.regs.x,
            8 => &mut self.regs.y,
            9 => &mut self.idl,
            10 => &mut self.abl,
            11 => &mut self.abh,
            _ => panic!("invalid register number"),
        }
    }
    
    pub fn execute(&mut self) {
        let uop = self.queue.pop_front().unwrap();
        let arg;
        match uop {
            MicroFn::ReadTo(regname) => arg = regname as u8,
            MicroFn::FetchTo(regname) => arg = regname as u8,
            MicroFn::Write(regname) => arg = regname as u8,
            MicroFn::Push(regname) => arg = regname as u8,
            MicroFn::BranchSet(flag) => arg = flag as u8,
            MicroFn::BranchClear(flag) => arg = flag as u8,
            MicroFn::SetFlag(flag) => arg = flag as u8,
            MicroFn::ClearFlag(flag) => arg = flag as u8,
            _ => arg = 0,
        }
        println!("executing micro-op {:?}. pc: {:#X}", uop, self.get_pc());
        let ufn :fn(&mut CPU, u8) = uop.into();
        
        (ufn)(self, arg);
    }
}
