extern crate red;

use std::fs::{read, OpenOptions};
use red::ines::*;
use red::nrom_mapper::*;
use red::cpu;
use std::rc::Rc;
use std::cell::RefCell;

#[macro_use]
extern crate slog;
use slog::Drain;

#[test]
fn donkey() {
    let log_path = "nestest.log";
    let file = OpenOptions::new()
      .create(true)
      .write(true)
      .truncate(true)
      .open(log_path)
      .unwrap();

    let file_decorator = slog_term::PlainSyncDecorator::new(file);
    let term_decorator = slog_term::TermDecorator::new().build();
    let file_drain = slog_term::FullFormat::new(file_decorator).build().fuse();
    let term_drain = slog_term::FullFormat::new(term_decorator).build().fuse();
    let term_drain = slog_async::Async::new(term_drain).build().fuse();
    let drain = slog::Duplicate::new(file_drain, term_drain).fuse();

    let log = slog::Logger::root(drain, o!());
    
    let path = "./donkey.nes";
    let mut contents = read(path).unwrap();
    //contents[16+16*1024-3] = 0xC0;
    //contents[16+16*1024-4] = 0x00;
    
    let rom = NesRom::from_bytes(&contents).unwrap();
    let mapper = create_mapper(rom);
    let mut cpu = cpu::CPU::with_mapper(Rc::new(RefCell::new(mapper)), log);
    
    println!("reset vector: {:#X}", cpu.get_pc());
    while true {
        cpu.execute();
    }
}

