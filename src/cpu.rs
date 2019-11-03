use crate::nrom_mapper::*;
use std::collections::VecDeque;
use slog::Logger;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ppu::*;

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
    Z = 0x02,
    I = 0x04,
    D = 0x08,
    V = 0x40,
    N = 0x80,
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
    mapper: Rc<RefCell<dyn Mapper>>,
    idl: u8,
    abl: u8,
    abh: u8,
    queue: VecDeque<MicroFn>,
    instruction_rom: Vec<Vec<MicroFn>>,
    ram: [u8; 0x800],
    root_logger: Logger,
    uop_logger: Logger,
    inst_logger: Logger,
    ppu: PPU,
    dma_latch: u8,
    dma_addr: u16,
    cycle_count: usize,
    nmi: bool,
    nmi_last: bool,
    nmi_occurred: bool,
}

#[derive(Debug, Copy, Clone)]
enum MicroFn {
    FetchAndDecode,
    FetchAndCopy,
    ReadAndCopy,
    BranchSet(Flags),
    BranchClear(Flags),
    ReadTo(RegNames),
    ReadAndIncrement(RegNames, RegNames),
    ReadIncNoWrap(RegNames),
    IndexWith(RegNames),
    FetchAndIndex(RegNames, RegNames),
    FetchIndexNoWrap(RegNames, RegNames),
    ReadAndUpdateFlags(RegNames),
    FetchTo(RegNames),
    FetchIndirect,
    FetchAndUpdateFlags(RegNames),
    Move(RegNames, RegNames),
    And(RegNames),
    Shr,
    ShrInMem,
    Shl,
    ShlInMem,
    Ror,
    RorInMem,
    Rol,
    RolInMem,
    OrA(RegNames),
    EOr(RegNames),
    Cmp(RegNames, RegNames),
    Add(RegNames),
    Sub(RegNames),
    Write(RegNames),
    Push(RegNames),
    Pull(RegNames),
    PullWithFlags(RegNames),
    SetFlag(Flags),
    ClearFlag(Flags),
    StepPC,
    Increment(RegNames),
    IncInMem,
    Decrement(RegNames),
    DecInMem,
    Test,
    Nop,
    ReadDMA,
    WriteDMA,
}

fn fetch_and_decode(cpu: &mut CPU, _: u8, _: u8) {
    //println!("PC:{:04X} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}", cpu.get_pc(), cpu.regs.a, cpu.regs.x, cpu.regs.y, cpu.regs.p & 0xCF | 0x20, cpu.regs.s);
    if cpu.nmi && !cpu.nmi_last {
        println!(">> NMI executing. P:{:#X}, PC:{:#X}, S:{:#X}", cpu.regs.p, cpu.get_pc(), cpu.regs.s);
        // push status then address
        cpu.set_byte(cpu.regs.s as u16 + 0x100, (cpu.get_pc() >> 8) as u8);
        cpu.regs.s = cpu.regs.s.wrapping_sub(1);
        cpu.set_byte(cpu.regs.s as u16 + 0x100, cpu.get_pc() as u8);
        cpu.regs.s = cpu.regs.s.wrapping_sub(1);
        cpu.set_byte(cpu.regs.s as u16 + 0x100, cpu.regs.p);
        cpu.regs.s = cpu.regs.s.wrapping_sub(1);
        
        // jump to NMI handler
        cpu.regs.pch = cpu.mapper.borrow().get_byte(0xFFFB);
        cpu.regs.pcl = cpu.mapper.borrow().get_byte(0xFFFA);
        cpu.queue.push_front(MicroFn::FetchAndDecode);
        cpu.nmi_occurred = true;
        cpu.nmi_last = cpu.nmi;
        return;
    }
    cpu.nmi_last = cpu.nmi;
    
    let op = cpu.fetch_byte();
    let uops = &cpu.instruction_rom[op as usize]; // Vec<MicroFn>
    assert!(uops.len() > 0, format!("invalid instruction {:#X?}!", op));
    //println!("Decoded operation {:#X}", op);
    for uop in uops.into_iter().rev() {
        cpu.queue.push_front(*uop);
    }
}

fn fetch_and_copy(cpu: &mut CPU, _: u8, _: u8) {
    cpu.regs.pch = cpu.fetch_byte();
    cpu.regs.pcl = cpu.idl;
}

impl From<MicroFn> for fn(&mut CPU, u8, u8) {
    fn from(x: MicroFn) -> fn(&mut CPU, u8, u8) {
        match x {
            MicroFn::ReadDMA => |cpu, _, _| {
                cpu.dma_latch = cpu.get_byte(cpu.dma_addr);
            },
            MicroFn::WriteDMA => |cpu, _, _| {
                cpu.ppu.write_oam(cpu.dma_addr as u8, cpu.dma_latch);
                cpu.dma_addr += 1;
            },
            MicroFn::FetchAndDecode => fetch_and_decode,
            MicroFn::FetchAndCopy => fetch_and_copy,
            MicroFn::ReadAndCopy => |cpu, _, _| {
                let val = cpu.get_byte(cpu.get_ab());
                cpu.regs.pch = val;
                cpu.regs.pcl = cpu.idl;
            },
            MicroFn::BranchSet(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum, _| {
                    if cpu.regs.p & flagnum != 0 {
                        let zext_pcl = cpu.regs.pcl as u16 as i16;
                        let sext_add = cpu.idl as i8 as i16;
                        let tmp_pcl = zext_pcl.wrapping_add(sext_add);
                        if tmp_pcl < 0 {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.regs.pch -= 1;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                            cpu.queue.push_front(MicroFn::Nop);
                        }
                        else if tmp_pcl > 0xFF {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.regs.pch += 1;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                            cpu.queue.push_front(MicroFn::Nop);
                        }
                        else {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                        }
                    }
                    else {
                        fetch_and_decode(cpu, 0, 0);
                    }
                }
            },
            MicroFn::BranchClear(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum, _| {
                    if cpu.regs.p & flagnum == 0 {
                        let zext_pcl = cpu.regs.pcl as u16 as i16;
                        let sext_add = cpu.idl as i8 as i16;
                        let tmp_pcl = zext_pcl.wrapping_add(sext_add);
                        if tmp_pcl < 0 {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.regs.pch -= 1;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                            cpu.queue.push_front(MicroFn::Nop);
                        }
                        else if tmp_pcl > 0xFF {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.regs.pch += 1;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                            cpu.queue.push_front(MicroFn::Nop);
                        }
                        else {
                            cpu.regs.pcl = tmp_pcl as u8;
                            cpu.queue.push_front(MicroFn::FetchAndDecode);
                        }
                    }
                    else {
                        fetch_and_decode(cpu, 0, 0);
                    }
                }
            },
            MicroFn::FetchIndirect => {
                |cpu, _, _| {
                    cpu.abh = cpu.get_byte(cpu.get_ab());
                    cpu.abl = cpu.idl;
                    let temp_abl = (cpu.abl as u16) + (cpu.regs.y as u16);
                    if temp_abl > 0xFFu16 {
                        cpu.queue.push_front(MicroFn::Nop); // page-crossing cycle
                        let tmp_hiadd = (cpu.abh as u16) + 1u16;
                        cpu.abh = tmp_hiadd as u8;
                    }
                    cpu.abl = temp_abl as u8;
                    //println!("FetchIndirect effective address {:#X}", cpu.get_ab());
                }
            },
            MicroFn::FetchTo(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    *cpu.get_reg_by_number(regnum) = cpu.fetch_byte();
                    if regnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::Cmp(regname, addrreg) => {
                let regnum = regname as u8;
                let addrnum = addrreg as u8;
                |cpu, regnum, addrnum| {
                    let loaded = cpu.fetch_byte_from(addrnum);
                    let regval = *cpu.get_reg_by_number(regnum);
                    let val = regval.wrapping_sub(loaded);
                    cpu.update_flag(Flags::C, regval >= val);
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::Shl => {
                |cpu, _, _| {
                    let oldval = cpu.regs.a;
                    let val = oldval << 1;
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x80 != 0);
                }
            },
            MicroFn::ShlInMem => {
                |cpu, _, _| {
                    let oldval = cpu.idl;
                    let val = oldval << 1;
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x80 != 0);
                }
            },
            MicroFn::Shr => {
                |cpu, _, _| {
                    let oldval = cpu.regs.a;
                    let val = oldval >> 1;
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x01 != 0);
                }
            },
            MicroFn::ShrInMem => {
                |cpu, _, _| {
                    let oldval = cpu.idl;
                    let val = oldval >> 1;
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x01 != 0);
                }
            },
            MicroFn::Rol => {
                |cpu, _, _| {
                    let oldval = cpu.regs.a;
                    let val = (oldval << 1) | (cpu.regs.p &0x01);
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x80 != 0);
                }
            },
            MicroFn::RolInMem => {
                |cpu, _, _| {
                    let oldval = cpu.idl;
                    let val = (oldval << 1) | (cpu.regs.p &0x01);
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x80 != 0);
                }
            },
            MicroFn::Ror => {
                |cpu, _, _| {
                    let oldval = cpu.regs.a;
                    let val = (oldval >> 1) | ((cpu.regs.p & 0x01) << 7);
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x01 != 0);
                }
            },
            MicroFn::RorInMem => {
                |cpu, _, _| {
                    let oldval = cpu.idl;
                    let val = (oldval >> 1) | ((cpu.regs.p & 0x01) << 7);
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                    cpu.update_flag(Flags::C, oldval & 0x01 != 0);
                }
            },
            MicroFn::OrA(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let loaded = cpu.fetch_byte_from(regnum);
                    let val = loaded | cpu.regs.a;
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::EOr(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let val = cpu.fetch_byte_from(regnum) ^ cpu.regs.a;
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::Add(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let pre_a = cpu.regs.a;
                    let loaded = cpu.fetch_byte_from(regnum);
                    add(cpu, pre_a, loaded);
                }
            },
            MicroFn::Sub(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let pre_a = cpu.regs.a;
                    let loaded = cpu.fetch_byte_from(regnum);
                    add(cpu, pre_a, !loaded);
                }
            },
            MicroFn::Move(to, from) => {
                let tonum = to as u8;
                let fromnum = from as u8;
                |cpu, tonum, fromnum| {
                    let fromval = *cpu.get_reg_by_number(fromnum);
                    let toreg = cpu.get_reg_by_number(tonum);
                    *toreg = fromval;
                    let val = *toreg;
                    if tonum != (RegNames::S as u8) { // dirty hack
                        cpu.update_flag(Flags::Z, val == 0);
                        cpu.update_flag(Flags::N, val & 0x80 != 0);
                    }
                }
            },
            MicroFn::And(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let val = cpu.fetch_byte_from(regnum) & cpu.regs.a;
                    cpu.regs.a = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::FetchAndUpdateFlags(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let loaded = cpu.fetch_byte();
                    *cpu.get_reg_by_number(regnum) = loaded;
                    cpu.update_flag(Flags::Z, loaded == 0);
                    cpu.update_flag(Flags::N, loaded & 0x80 != 0);
                    if regnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::ReadTo(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let loaded = cpu.get_byte(cpu.get_ab());
                    *cpu.get_reg_by_number(regnum) = loaded;
                    if regnum == RegNames::A as u8 {
                        //println!("Writing {:#X} to A", loaded);
                    }
                    cpu.update_flag(Flags::Z, loaded == 0);
                    cpu.update_flag(Flags::N, loaded & 0x80 != 0);
                    if regnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::ReadAndIncrement(toreg, fromreg) => {
                let toregnum = toreg as u8;
                let fromregnum = fromreg as u8;
                |cpu, toregnum, fromregnum| {
                    let val = cpu.fetch_byte_from(fromregnum);
                    *cpu.get_reg_by_number(toregnum) = val;
                    if toregnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::ReadIncNoWrap(toreg) => {
                let toregnum = toreg as u8;
                |cpu, toregnum, fromregnum| {
                    let val = cpu.get_byte(cpu.get_ab());
                    *cpu.get_reg_by_number(toregnum) = val;
                    let tmp_abl = (cpu.abl as u16) + 1u16;
                    cpu.abl = tmp_abl as u8;
                    if toregnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::IndexWith(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    cpu.idl = cpu.idl.wrapping_add(*cpu.get_reg_by_number(regnum));
                }
            },
            MicroFn::FetchAndIndex(toname, indexname) => {
                let tonum = toname as u8;
                let indexnum = indexname as u8;
                |cpu, tonum, indexnum| {
                    assert!(tonum == RegNames::ABH as u8);
                    let index_reg_val = *cpu.get_reg_by_number(indexnum);
                    *cpu.get_reg_by_number(tonum) = cpu.fetch_byte();
                    let tmp_abl = (cpu.abl as u16) + (index_reg_val as u16);
                    if tmp_abl > 0xFFu16 {
                        let tmp_hiadd = (cpu.abh as u16) + 1u16;
                        cpu.abh = tmp_hiadd as u8;
                        cpu.queue.push_front(MicroFn::Nop);
                    }
                    cpu.abl = tmp_abl as u8;
                }
            },
            MicroFn::FetchIndexNoWrap(toname, indexname) => {
                let tonum = toname as u8;
                let indexnum = indexname as u8;
                |cpu, tonum, indexnum| {
                    assert!(tonum == RegNames::ABL as u8);
                    let index_reg_val = *cpu.get_reg_by_number(indexnum);
                    *cpu.get_reg_by_number(tonum) = cpu.fetch_byte();
                    let tmp_abl = ((cpu.abl as u16) + (index_reg_val as u16)) as u8;
                    cpu.abl = tmp_abl as u8;
                    cpu.abh = 0;
                }
            },
            MicroFn::ReadAndUpdateFlags(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let loaded = cpu.get_byte(cpu.get_ab());
                    *cpu.get_reg_by_number(regnum) = loaded;
                    cpu.update_flag(Flags::Z, loaded == 0);
                    cpu.update_flag(Flags::N, loaded & 0x80 != 0);
                    if regnum == RegNames::ABL as u8 {
                        cpu.abh = 0;
                    }
                }
            },
            MicroFn::Write(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let val: u8 = *cpu.get_reg_by_number(regnum);
                    if cpu.get_ab() == 0x2007 {
                    //println!("Writing to address {:#X}, value {:#X}, A {:#X}", cpu.get_ab(), val, cpu.regs.a);
                    }
                    cpu.set_byte(cpu.get_ab(), val);
                }
            },
            MicroFn::Push(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let val: u8 = *cpu.get_reg_by_number(regnum);
                    cpu.set_byte(cpu.regs.s as u16 + 0x100, val);
                    cpu.regs.s = cpu.regs.s.wrapping_sub(1);
                }
            },
            MicroFn::Pull(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    cpu.regs.s = cpu.regs.s.wrapping_add(1);
                    let val = cpu.get_byte(cpu.regs.s as u16 + 0x100);
                    *cpu.get_reg_by_number(regnum) = val;
                }
            },
            MicroFn::PullWithFlags(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    cpu.regs.s = cpu.regs.s.wrapping_add(1);
                    let val = cpu.get_byte(cpu.regs.s as u16 + 0x100);
                    *cpu.get_reg_by_number(regnum) = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::StepPC => |cpu, _, _| {
                let tmp_pc = cpu.regs.pcl as u16 + 1;
                if tmp_pc > 0xFF {
                    cpu.regs.pch += 1;
                }
                cpu.regs.pcl = tmp_pc as u8;
            },
            MicroFn::Increment(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let reg = cpu.get_reg_by_number(regnum);
                    *reg = reg.wrapping_add(1);
                    let val = *reg;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::IncInMem => {
                |cpu, _, _| {
                    let reg = cpu.idl;
                    let val = reg.wrapping_add(1);
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::Decrement(regname) => {
                let regnum = regname as u8;
                |cpu, regnum, _| {
                    let reg = cpu.get_reg_by_number(regnum);
                    *reg = reg.wrapping_sub(1);
                    let val = *reg;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::DecInMem => {
                |cpu, _, _| {
                    let reg = cpu.idl;
                    let val = reg.wrapping_sub(1);
                    cpu.ram[cpu.get_ab() as usize] = val;
                    cpu.update_flag(Flags::Z, val == 0);
                    cpu.update_flag(Flags::N, val & 0x80 != 0);
                }
            },
            MicroFn::SetFlag(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum, _| cpu.regs.p |= flagnum
            },
            MicroFn::ClearFlag(flag) => {
                let flagnum = flag as u8;
                |cpu, flagnum, _| cpu.regs.p &= !flagnum
            },
            MicroFn::Test => |cpu, _, _| {
                let loaded = cpu.get_byte(cpu.get_ab());
                let tmp_p = cpu.regs.p & 0x3D;
                let zero = if loaded & cpu.regs.a == 0 { 0x02 } else { 0 };
                cpu.regs.p = tmp_p | (loaded & 0xC0) | zero;
            },
            MicroFn::Nop => |_, _, _| {},
        }
    }
}

fn add(cpu: &mut CPU, pre_a: u8, loaded: u8) {
    let val = (pre_a as u16).wrapping_add(loaded as u16).wrapping_add((cpu.regs.p as u16) & 0x01);
    cpu.regs.a = val as u8;
    cpu.update_flag(Flags::Z, cpu.regs.a == 0);
    cpu.update_flag(Flags::N, cpu.regs.a & 0x80 != 0);
    cpu.update_flag(Flags::C, val > 0xFFu16);
    cpu.update_flag(Flags::V, (pre_a & 0x80 == loaded & 0x80) && (pre_a & 0x80) != ((val as u8) & 0x80));
}

const JMP_ABS :u8 = 0x4C;
const JMP_IND :u8 = 0x6C;
const AND_IMM :u8 = 0x29;
const ORA_IMM :u8 = 0x09;
const EOR_IMM :u8 = 0x49;
const ADC_IMM :u8 = 0x69;
const SBC_IMM :u8 = 0xE9;
const CMP_IMM :u8 = 0xC9;
const CPY_IMM :u8 = 0xC0;
const CPX_IMM :u8 = 0xE0;
const LDX_IMM :u8 = 0xA2;
const AND_IND_X :u8 = 0x21;
const AND_IND_Y :u8 = 0x31;
const ORA_IND_X :u8 = 0x01;
const ORA_IND_Y :u8 = 0x11;
const EOR_IND_X :u8 = 0x41;
const EOR_IND_Y :u8 = 0x51;
const ADC_IND_X :u8 = 0x61;
const ADC_IND_Y :u8 = 0x71;
const SBC_IND_X :u8 = 0xE1;
const SBC_IND_Y :u8 = 0xF1;
const CMP_IND_X :u8 = 0xC1;
const CMP_IND_Y :u8 = 0xD1;
const LDA_IND_X :u8 = 0xA1;
const AND_ZPG :u8 = 0x25;
const AND_ZPG_X :u8 = 0x35;
const AND_ABS :u8 = 0x2D;
const AND_ABS_X :u8 = 0x3D;
const AND_ABS_Y :u8 = 0x39;
const ORA_ZPG :u8 = 0x05;
const ORA_ZPG_X :u8 = 0x15;
const ORA_ABS :u8 = 0x0D;
const ORA_ABS_X :u8 = 0x1D;
const ORA_ABS_Y :u8 = 0x19;
const EOR_ZPG :u8 = 0x45;
const EOR_ZPG_X :u8 = 0x55;
const EOR_ABS :u8 = 0x4D;
const EOR_ABS_X :u8 = 0x5D;
const EOR_ABS_Y :u8 = 0x59;
const ADC_ZPG :u8 = 0x65;
const ADC_ZPG_X :u8 = 0x75;
const ADC_ABS :u8 = 0x6D;
const ADC_ABS_X :u8 = 0x7D;
const ADC_ABS_Y :u8 = 0x79;
const CMP_ZPG :u8 = 0xC5;
const CMP_ZPG_X :u8 = 0xD5;
const CMP_ABS :u8 = 0xCD;
const CMP_ABS_X :u8 = 0xDD;
const CMP_ABS_Y :u8 = 0xD9;
const CPX_ZPG :u8 = 0xE4;
const CPX_ABS :u8 = 0xEC;
const CPY_ZPG :u8 = 0xC4;
const CPY_ABS :u8 = 0xCC;
const SBC_ZPG :u8 = 0xE5;
const SBC_ZPG_X :u8 = 0xF5;
const SBC_ABS :u8 = 0xED;
const SBC_ABS_X :u8 = 0xFD;
const SBC_ABS_Y :u8 = 0xF9;
const LSR_ZPG :u8 = 0x46;
const LSR_ZPG_X :u8 = 0x56;
const LSR_ABS :u8 = 0x4E;
const LSR_ABS_X :u8 = 0x5E;
const LSR_ACC :u8 = 0x4A;
const ROR_ACC :u8 = 0x6A;
const ROR_ZPG :u8 = 0x66;
const ROR_ZPG_X :u8 = 0x76;
const ROR_ABS :u8 = 0x6E;
const ROR_ABS_X :u8 = 0x7E;
const ASL_ACC :u8 = 0x0A;
const ASL_ZPG :u8 = 0x06;
const ASL_ZPG_X :u8 = 0x16;
const ASL_ABS :u8 = 0x0E;
const ASL_ABS_X :u8 = 0x1E;
const ROL_ACC :u8 = 0x2A;
const ROL_ZPG :u8 = 0x26;
const ROL_ZPG_X :u8 = 0x36;
const ROL_ABS :u8 = 0x2E;
const ROL_ABS_X :u8 = 0x3E;
const INX :u8 = 0xE8;
const INC_ZPG :u8 = 0xE6;
const INC_ZPG_X :u8 = 0xF6;
const INC_ABS :u8 = 0xEE;
const INC_ABS_X :u8 = 0xFE;
const DEX :u8 = 0xCA;
const DEC_ZPG :u8 = 0xC6;
const DEC_ZPG_X :u8 = 0xD6;
const DEC_ABS :u8 = 0xCE;
const DEC_ABS_X :u8 = 0xDE;
const LDY_IMM :u8 = 0xA0;
const LDY_ABS :u8 = 0xAC;
const LDY_ABS_X :u8 = 0xBC;
const INY :u8 = 0xC8;
const DEY :u8 = 0x88;
const TAX :u8 = 0xAA;
const TSX :u8 = 0xBA;
const TXS :u8 = 0x9A;
const TAY :u8 = 0xA8;
const TXA :u8 = 0x8A;
const TYA :u8 = 0x98;
const LDA_ABS :u8 = 0xAD;
const LDA_ABS_X :u8 = 0xBD;
const LDA_ABS_Y :u8 = 0xB9;
const LDA_IND_Y :u8 = 0xB1;
const LDX_ABS :u8 = 0xAE;
const LDX_ABS_Y :u8 = 0xBE;
const STA_ABS :u8 = 0x8D;
const STA_ABS_X :u8 = 0x9D;
const STA_ABS_Y :u8 = 0x99;
const STA_IND_X :u8 = 0x81;
const STA_IND_Y :u8 = 0x91;
const STX_ABS :u8 = 0x8E;
const STX_ZPG :u8 = 0x86;
const STX_ZPG_Y :u8 = 0x96;
const STY_ZPG :u8 = 0x84;
const STY_ZPG_X :u8 = 0x94;
const STY_ABS :u8 = 0x8C;
const STA_ZPG :u8 = 0x85;
const STA_ZPG_X :u8 = 0x95;
const BIT_ZPG :u8 = 0x24;
const BIT_ABS :u8 = 0x2C;
const JSR :u8 = 0x20;
const PHP :u8 = 0x08;
const PLP :u8 = 0x28;
const PHA :u8 = 0x48;
const PLA :u8 = 0x68;
const RTS :u8 = 0x60;
const RTI :u8 = 0x40;
const NOP :u8 = 0xEA;
const NOP_04 :u8 = 0x04;
const NOP_44 :u8 = 0x44;
const NOP_64 :u8 = 0x64;
const NOP_0C :u8 = 0x0C;
const NOP_14 :u8 = 0x14;
const NOP_34 :u8 = 0x34;
const NOP_54 :u8 = 0x54;
const NOP_74 :u8 = 0x74;
const NOP_D4 :u8 = 0xD4;
const NOP_F4 :u8 = 0xF4;
const NOP_1A :u8 = 0x1A;
const NOP_3A :u8 = 0x3A;
const NOP_5A :u8 = 0x5A;
const NOP_7A :u8 = 0x7A;
const NOP_DA :u8 = 0xDA;
const NOP_FA :u8 = 0xFA;
const NOP_80 :u8 = 0x80;
const NOP_1C :u8 = 0x1C;
const NOP_3C :u8 = 0x3C;
const NOP_5C :u8 = 0x5C;
const NOP_7C :u8 = 0x7C;
const NOP_DC :u8 = 0xDC;
const NOP_FC :u8 = 0xFC;
const SEI :u8 = 0x78;
const SED :u8 = 0xF8;
const SEC :u8 = 0x38;
const CLC :u8 = 0x18;
const CLD :u8 = 0xD8;
const CLV :u8 = 0xB8;
const BCS :u8 = 0xB0;
const BCC :u8 = 0x90;
const BEQ :u8 = 0xF0;
const BNE :u8 = 0xD0;
const BMI :u8 = 0x30;
const BVS :u8 = 0x70;
const BVC :u8 = 0x50;
const BPL :u8 = 0x10;
const LDA_IMM :u8 = 0xA9;
const LDA_ZPG :u8 = 0xA5;
const LDA_ZPG_X :u8 = 0xB5;
const LDY_ZPG :u8 = 0xA4;
const LDY_ZPG_X :u8 = 0xB4;
const LDX_ZPG :u8 = 0xA6;
const LDX_ZPG_Y :u8 = 0xB6;

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
        
        result[JMP_IND as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::ReadAndCopy,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_IMM as usize] = vec![ 
            MicroFn::FetchAndUpdateFlags(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDY_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDY_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_ZPG_Y as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::Y),
            MicroFn::ReadTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::ReadTo(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::ReadTo(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_IMM as usize] = vec![ 
            MicroFn::FetchAndUpdateFlags(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INX as usize] = vec![ 
            MicroFn::Increment(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INC_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::IncInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INC_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::IncInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INC_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::IncInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INC_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::IncInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEX as usize] = vec![ 
            MicroFn::Decrement(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEC_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::DecInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEC_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::DecInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEC_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::DecInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEC_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::DecInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDY_IMM as usize] = vec![ 
            MicroFn::FetchAndUpdateFlags(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDY_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDY_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[INY as usize] = vec![ 
            MicroFn::Increment(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[DEY as usize] = vec![ 
            MicroFn::Decrement(RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TAX as usize] = vec![ 
            MicroFn::Move(RegNames::X, RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TSX as usize] = vec![ 
            MicroFn::Move(RegNames::X, RegNames::S),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TXS as usize] = vec![ 
            MicroFn::Move(RegNames::S, RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TXA as usize] = vec![ 
            MicroFn::Move(RegNames::A, RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TAY as usize] = vec![ 
            MicroFn::Move(RegNames::Y, RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[TYA as usize] = vec![ 
            MicroFn::Move(RegNames::A, RegNames::Y),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_IMM as usize] = vec![ 
            MicroFn::Cmp(RegNames::A, RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[CPX_IMM as usize] = vec![ 
            MicroFn::Cmp(RegNames::X, RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CPY_IMM as usize] = vec![ 
            MicroFn::Cmp(RegNames::Y, RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_IMM as usize] = vec![ 
            MicroFn::And(RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::And(RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_IMM as usize] = vec![ 
            MicroFn::OrA(RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::OrA(RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ORA_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::OrA(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[AND_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::And(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CMP_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::Cmp(RegNames::A, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CPX_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Cmp(RegNames::X, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CPX_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Cmp(RegNames::X, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CPY_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Cmp(RegNames::Y, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[CPY_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Cmp(RegNames::Y, RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDA_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::ReadAndUpdateFlags(RegNames::A),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_IMM as usize] = vec![ 
            MicroFn::EOr(RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::EOr(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[EOR_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::EOr(RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_IMM as usize] = vec![ 
            MicroFn::Add(RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::Add(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ADC_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::Add(RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[ASL_ACC as usize] = vec![ 
            MicroFn::Shl,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ASL_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShlInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ASL_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShlInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ASL_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShlInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ASL_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShlInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LSR_ACC as usize] = vec![ 
            MicroFn::Shr,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LSR_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShrInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LSR_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShrInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LSR_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShrInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LSR_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::ShrInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROL_ACC as usize] = vec![ 
            MicroFn::Rol,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROL_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RolInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROL_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RolInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROL_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RolInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROL_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RolInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROR_ACC as usize] = vec![ 
            MicroFn::Ror,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROR_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RorInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROR_ZPG_X as usize] = vec![ 
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RorInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROR_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RorInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[ROR_ABS_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::RorInMem,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_IMM as usize] = vec![ 
            MicroFn::Sub(RegNames::PCL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_IND_X as usize] = vec![ 
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::Sub(RegNames::ABL),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[SBC_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::Sub(RegNames::ABL),
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::ReadTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[LDX_ABS_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::ReadTo(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[STX_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Write(RegNames::X),
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[BIT_ZPG as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Test,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[BIT_ABS as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Test,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_ZPG as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_ZPG_X as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_IND_X as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::IndexWith(RegNames::X),
            MicroFn::ReadAndIncrement(RegNames::ABL, RegNames::IDL),
            MicroFn::ReadAndIncrement(RegNames::ABH, RegNames::IDL),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_IND_Y as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::ReadIncNoWrap(RegNames::IDL),
            MicroFn::FetchIndirect,
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_ABS as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_ABS_X as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STA_ABS_Y as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::Y),
            MicroFn::Write(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[STX_ZPG as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Write(RegNames::X),
            MicroFn::FetchAndDecode,
        ];
        
        result[STX_ZPG_Y as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::Y),
            MicroFn::Write(RegNames::X),
            MicroFn::FetchAndDecode,
        ];
        
        result[STY_ZPG as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::Write(RegNames::Y),
            MicroFn::FetchAndDecode,
        ];
        
        result[STY_ZPG_X as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::Write(RegNames::Y),
            MicroFn::FetchAndDecode,
        ];
        
        result[STY_ABS as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Write(RegNames::Y),
            // fetch and decode next instruction
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
        
        result[PHP as usize] = vec![
            MicroFn::Nop,
            MicroFn::Push(RegNames::P),
            MicroFn::FetchAndDecode,
        ];
        
        result[PHA as usize] = vec![
            MicroFn::Nop,
            MicroFn::Push(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[PLP as usize] = vec![
            MicroFn::Nop,
            MicroFn::Nop,
            MicroFn::Pull(RegNames::P),
            MicroFn::FetchAndDecode,
        ];
        
        result[PLA as usize] = vec![
            MicroFn::Nop,
            MicroFn::Nop,
            MicroFn::PullWithFlags(RegNames::A),
            MicroFn::FetchAndDecode,
        ];
        
        result[RTS as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::Pull(RegNames::PCL),
            MicroFn::Pull(RegNames::PCH),
            MicroFn::StepPC,
            MicroFn::FetchAndDecode,
        ];
        
        result[RTI as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::Pull(RegNames::P),
            MicroFn::Pull(RegNames::PCL),
            MicroFn::Pull(RegNames::PCH),
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_04 as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_44 as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_64 as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_0C as usize] = vec![
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchTo(RegNames::ABH),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_14 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_34 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_54 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_74 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_D4 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_F4 as usize] = vec![
            MicroFn::FetchIndexNoWrap(RegNames::ABL, RegNames::X),
            MicroFn::ReadTo(RegNames::IDL),
            MicroFn::Write(RegNames::IDL),
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_1A as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_3A as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_5A as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_7A as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_DA as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_FA as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_80 as usize] = vec![
            MicroFn::StepPC,
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_1C as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_3C as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_5C as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_7C as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_DC as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP_FC as usize] = vec![ 
            MicroFn::FetchTo(RegNames::ABL),
            MicroFn::FetchAndIndex(RegNames::ABH, RegNames::X),
            MicroFn::Nop,
            // fetch and decode next instruction
            MicroFn::FetchAndDecode,
        ];
        
        result[NOP as usize] = vec![
            MicroFn::Nop,
            MicroFn::FetchAndDecode,
        ];
        
        result[SED as usize] = vec![
            MicroFn::SetFlag(Flags::D),
            MicroFn::FetchAndDecode,
        ];
        
        result[SEI as usize] = vec![
            MicroFn::SetFlag(Flags::I),
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
        
        result[CLD as usize] = vec![
            MicroFn::ClearFlag(Flags::D),
            MicroFn::FetchAndDecode,
        ];
        
        result[CLV as usize] = vec![
            MicroFn::ClearFlag(Flags::V),
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
        
        result[BEQ as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchSet(Flags::Z),
        ];
        
        result[BMI as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchSet(Flags::N),
        ];
        
        result[BNE as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchClear(Flags::Z),
        ];
        
        result[BVS as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchSet(Flags::V),
        ];
        
        result[BVC as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchClear(Flags::V),
        ];
        
        result[BPL as usize] = vec![
            MicroFn::FetchTo(RegNames::IDL),
            MicroFn::BranchClear(Flags::N),
        ];
        
        result
    }
    
    pub fn get_ab(&self) -> u16 {
        (self.abl as u16) | ((self.abh as u16) << 8)
    }
    
    pub fn with_mapper(mapper: Rc<RefCell<dyn Mapper>>, logger: Logger) -> CPU {
        let reset_vector = ((mapper.borrow().get_byte(0xFFFD) as u16) << 8) | (mapper.borrow().get_byte(0xFFFC) as u16);
        let mut queue = VecDeque::new();
        queue.push_front(MicroFn::FetchAndDecode);
        
        let uop_logger = logger.new(o!());
        let inst_logger = logger.new(o!());
        
        let ppu = PPU::with_mapper(mapper.clone());

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
            root_logger: logger,
            uop_logger,
            inst_logger,
            ppu,
            dma_latch: 0,
            dma_addr: 0,
            cycle_count: 0,
            nmi: false,
            nmi_last: false,
            nmi_occurred: false,
        }
    }
    
    pub fn get_pc(&self) -> u16 {
        ((self.regs.pch as u16) << 8) | (self.regs.pcl as u16)
    }
    
    pub fn get_byte(&mut self, addr: u16) -> u8 {
        if addr >= 0x4020 {
            self.mapper.borrow().get_byte(addr)
        }
        else if addr >= 0x2000u16 && addr < 0x2008u16 {
            *self.ppu.get_reg(addr)
        }
        else if addr >= 0x4000u16 && addr < 0x4018u16 {
            //println!("Read from unimplemented address {:#X}", addr);
            return 0;
        }
        else {
            assert!(addr < 0x2000);
            let effective_addr = addr & 0x7FF;
            self.ram[effective_addr as usize]
        }
    }
    
    pub fn fetch_byte_from(&mut self, regnum: u8) -> u8 {
        let result;
        if regnum == RegNames::PCL as u8 {
            result = self.get_byte(self.get_pc());
            if self.regs.pcl == 0xFF {
                self.regs.pch += 1;
                self.regs.pcl = 0;
            }
            else {
                self.regs.pcl += 1;
            }
        }
        else if regnum == RegNames::ABL as u8 {
            result = self.get_byte(self.get_ab());
            if self.abl == 0xFF {
                self.abh += 1;
                self.abl = 0;
            }
            else {
                self.abl += 1;
            }
        }
        else {
            let regval = *self.get_reg_by_number(regnum);
            result = self.get_byte(regval as u16);
            *self.get_reg_by_number(regnum) = regval.wrapping_add(1);
        }
        result
    }
    
    pub fn fetch_byte(&mut self) -> u8 {
        let result = self.get_byte(self.get_pc());
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
        if addr == 2 || addr == 3 {
            //println!("Write to address {} with value {}", addr, value);
            //panic!();
        }
        let effective_addr;
        if addr < 0x2000u16 {
            effective_addr = addr & 0x7FF;
        }
        else if addr >= 0x2000u16 && addr < 0x2008u16 {
            self.ppu.set_reg(addr, value);
            return;
        }
        else if addr == 0x4014u16 {
            // no actual write to PPU - port on CPU
            for i in 0..256 {
                self.queue.push_front(MicroFn::WriteDMA);
                self.queue.push_front(MicroFn::ReadDMA);
            }
            self.queue.push_front(MicroFn::Nop);
            // if odd cycle another NOP
            if self.cycle_count & 1 > 0 {
                self.queue.push_front(MicroFn::Nop);
            }
            self.dma_addr = (value as u16) << 8;
            return;
        }
        else if addr >= 0x4000u16 && addr < 0x4018u16 {
            //println!("Write to unimplemented address {:#X}", addr);
            return;
        }
        else {
            panic!("address not implemented: {:#X?}", addr);
        }
        self.ram[effective_addr as usize] = value;
    }
    
    fn update_flag(&mut self, flag: Flags, cond: bool) {
        if cond {
            self.regs.p |= flag as u8;
        }
        else {
            self.regs.p &= !(flag as u8);
        }
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
        self.nmi_occurred = false;
        while !self.nmi_occurred {
            let uop = self.queue.pop_front().unwrap();
            let arg;
            match uop {
                MicroFn::ReadTo(regname) => arg = regname as u8,
                MicroFn::FetchAndIndex(regname, _) => arg = regname as u8,
                MicroFn::FetchIndexNoWrap(regname, _) => arg = regname as u8,
                MicroFn::ReadAndIncrement(regname, _) => arg = regname as u8,
                MicroFn::ReadIncNoWrap(regname) => arg = regname as u8,
                MicroFn::IndexWith(regname) => arg = regname as u8,
                MicroFn::Cmp(regname, _) => arg = regname as u8,
                MicroFn::OrA(regname) => arg = regname as u8,
                MicroFn::EOr(regname) => arg = regname as u8,
                MicroFn::And(regname) => arg = regname as u8,
                MicroFn::Add(regname) => arg = regname as u8,
                MicroFn::Sub(regname) => arg = regname as u8,
                MicroFn::ReadAndUpdateFlags(regname) => arg = regname as u8,
                MicroFn::FetchTo(regname) => arg = regname as u8,
                MicroFn::FetchAndUpdateFlags(regname) => arg = regname as u8,
                MicroFn::Write(regname) => arg = regname as u8,
                MicroFn::Increment(regname) => arg = regname as u8,
                MicroFn::Decrement(regname) => arg = regname as u8,
                MicroFn::Push(regname) => arg = regname as u8,
                MicroFn::Pull(regname) => arg = regname as u8,
                MicroFn::PullWithFlags(regname) => arg = regname as u8,
                MicroFn::BranchSet(flag) => arg = flag as u8,
                MicroFn::BranchClear(flag) => arg = flag as u8,
                MicroFn::SetFlag(flag) => arg = flag as u8,
                MicroFn::ClearFlag(flag) => arg = flag as u8,
                MicroFn::Move(regname, _) => arg = regname as u8,
                _ => arg = 0,
            }
            let arg2;
            match uop {
                MicroFn::ReadTo(regname) => arg2 = 0,
                MicroFn::ReadAndIncrement(_, regname) => arg2 = regname as u8,
                MicroFn::FetchAndIndex(_, regname) => arg2 = regname as u8,
                MicroFn::FetchIndexNoWrap(_, regname) => arg2 = regname as u8,
                MicroFn::Cmp(_, regname) => arg2 = regname as u8,
                MicroFn::ReadAndUpdateFlags(regname) => arg2 = 0,
                MicroFn::FetchTo(regname) => arg2 = 0,
                MicroFn::FetchAndUpdateFlags(regname) => arg2 = 0,
                MicroFn::Write(regname) => arg2 = 0,
                MicroFn::Increment(regname) => arg2 = 0,
                MicroFn::Decrement(regname) => arg2 = 0,
                MicroFn::Push(regname) => arg2 = 0,
                MicroFn::Pull(regname) => arg2 = 0,
                MicroFn::PullWithFlags(regname) => arg2 = 0,
                MicroFn::BranchSet(flag) => arg2 = 0,
                MicroFn::BranchClear(flag) => arg2 = 0,
                MicroFn::SetFlag(flag) => arg2 = 0,
                MicroFn::ClearFlag(flag) => arg2 = 0,
                MicroFn::Move(_, regname) => arg2 = regname as u8,
                _ => arg2 = 0,
            }
            
            
            let pc = self.get_pc();
            //println!("PC: {:#X}, A: {:#X}, X: {:#X}, Y: {:#X}", pc, self.regs.a, self.regs.x, self.regs.y);
            let ufn :fn(&mut CPU, u8, u8) = uop.into();
            
            (ufn)(self, arg, arg2);
            
            let nmi = self.ppu.run();
            
            self.nmi = nmi;
            
            self.cycle_count = self.cycle_count.wrapping_add(1);
        }
        println!("Drawing framebuffer.");
        
    }
    
    pub fn get_framebuffer(&self) -> &[u32] {
        &self.ppu.framebuffer
    }
}
