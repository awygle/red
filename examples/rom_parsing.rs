use gumdrop::Options;
use red::ines::*;

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
    
    dbg!(rom);
    Ok(())
}

