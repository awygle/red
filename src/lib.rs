use gumdrop::Options;

#[macro_use]
extern crate slog;

pub mod ines;
pub mod nrom_mapper;
pub mod cpu;
pub mod ppu;

#[derive(Debug, Options)]
struct RedOptions {
    #[options(help = "the name of the ROM to execute")]
    rom_path: String
}

fn main() {
    let opts = RedOptions::parse_args_default_or_exit();
    
    println!("{:#?}", opts);
}
