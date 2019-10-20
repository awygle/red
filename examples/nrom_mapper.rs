use gumdrop::Options;
use red::ines::*;
use red::nrom_mapper::*;

#[derive(Debug, Options)]
struct RedOptions {
    #[options(help = "the name of the ROM to execute")]
    #[options(free)]
    rom_path: String
}

fn main() -> std::io::Result<()> {
    let opts = RedOptions::parse_args_default_or_exit();
    
    println!("opening rom {}", opts.rom_path);

    let rom = NesRom::from_file(&opts.rom_path)?;
    let mapper = create_mapper(rom);
    
    let reset_vector = ((mapper.get_byte(0xFFFD) as u16) << 8) | (mapper.get_byte(0xFFFC) as u16);
    println!("reset vector: {:#X}", reset_vector);
    
    Ok(())
}

