use crate::nrom_mapper::*;
use std::collections::VecDeque;
use slog::Logger;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::File;
use std::io::Write;
use std::cell::Cell;

const ntsc_lookup: [u8; 256] = [
084, 084, 084, 255,  000, 030, 116, 255,  008, 016, 144, 255,  048, 000, 136, 255, 
068, 000, 100, 255,  092, 000, 048, 255,  084, 004, 000, 255,  060, 024, 000, 255, 
032, 042, 000, 255,  008, 058, 000, 255,  000, 064, 000, 255,  000, 060, 000, 255, 
000, 050, 060, 255,  000, 000, 000, 255,  000, 000, 000, 255,  000, 000, 000, 255, 
    
152, 150, 152, 255,  008, 076, 196, 255,  048, 050, 236, 255,  092, 030, 228, 255, 
136, 020, 176, 255,  160, 020, 100, 255,  152, 034, 032, 255,  120, 060, 000, 255, 
084, 090, 000, 255,  040, 114, 000, 255,  008, 124, 000, 255,  000, 118, 040, 255,     
000, 102, 120, 255,  000, 000, 000, 255,  000, 000, 000, 255,  000, 000, 000, 255, 
    
236, 238, 236, 255,  076, 154, 236, 255,  120, 124, 236, 255,  176, 098, 236, 255, 
228, 084, 236, 255,  236, 088, 180, 255,  236, 106, 100, 255,  212, 136, 032, 255,   
160, 170, 000, 255,  116, 196, 000, 255,  076, 208, 032, 255,  056, 204, 108, 255, 
056, 180, 204, 255,  060, 060, 060, 255,  000, 000, 000, 255,  000, 000, 000, 255, 
    
236, 238, 236, 255,  168, 204, 236, 255,  188, 188, 236, 255,  212, 178, 236, 255, 
236, 174, 236, 255,  236, 174, 212, 255,  236, 180, 176, 255,  228, 196, 144, 255, 
204, 210, 120, 255,  180, 222, 120, 255,  168, 226, 144, 255,  152, 226, 180, 255,   
160, 214, 228, 255,  160, 162, 160, 255,  000, 000, 000, 255,  000, 000, 000, 255, 
];

struct BGState {
    op: BGOp,
    tile: i8,
    scanline: u16,
}

#[derive(Debug, PartialEq)]
pub enum BGOp {
    WaitMem,
    Nop,
    SetVBlank,
    FetchNT,
    FetchAT,
    FetchBGLow,
    FetchBGHigh,
    IncHoriz,
}

struct SpriteState {
    op: SpriteOp,
    cycle: u16,
    scanline: u16,
}

#[derive(Debug, PartialEq)]
pub enum SpriteOp {
    WaitMem,
    Nop,
    ClearOAM,
    ReadOAM,
    TestSprite,
    WriteOAM,
    CheckOverflow,
    ReadOAMBugged,
    FetchNT,
    FetchAT,
    FetchSpriteLow,
    FetchSpriteHigh,
    IncHoriz,
}

fn drawy_bits(ppu: &mut PPU, cycle: u16, scanline: u16) {
    if cycle >= 256 || scanline >= 240 {
        //if cycle == 256 && scanline == 240 {
        //    let mut file = File::create(format!("framebuffer_{}.ppm", ppu.cycle_count)).unwrap();
        //    write!(&mut file, "P3\n{} {}\n255\n", 256, 240);
        //    for y in 0..240 {
        //        for x in 0..256 {
        //            let pixel = ppu.framebuffer[x*240+y];
        //            for i in 1..4 {
        //                write!(&mut file, "{} ", pixel.to_be_bytes()[i]);
        //            }
        //        }
        //        write!(&mut file, "\n");
        //    }
        //    if ppu.cycle_count > 4_000_000 {
        //        panic!();
        //    }
        //}
        return;
    }
    
    
    // TODO add fine_x
    //println!("PLL:{:#X} PLH:{:#X} AL:{:#X}", 
    //         ppu.bg_pattern_latches[0],
    //         ppu.bg_pattern_latches[1],
    //         ppu.bg_attribute_latches
             
    // Background
    let shift = (7 - ((cycle - 0) % 8));
    let bg_index_low = ppu.bg_pattern_latches[0] >> shift & 0x01;
    let bg_index_high = ppu.bg_pattern_latches[1] >> shift & 0x01;
    let bg_index = bg_index_low | (bg_index_high << 1);
    let palette_index = ppu.bg_attribute_latches[0];
    assert!(palette_index < 4);
    
    // Sprites
    for i in 0..8 {
        //println!("Checking sprite {}", i);
        let x = ppu.oam_xpos_regs[i] as u16;
        if x <= cycle && x + 8 > cycle {
            // sprite is active
            println!("sprite {}, scanline {}, cycle {}, x {}", i, scanline, cycle, x);
            let shift = cycle - x;
            //println!("Using sprite address {:#X}", addr);
            let low = ppu.oam_pattern_regs[i][0];
            let high = ppu.oam_pattern_regs[i][1];
            let low_bit = low >> (7 - shift) & 0x01;
            let high_bit = high >> (7 - shift) & 0x01;
            let sprite_index = low_bit | (high_bit << 1);
            let sprite_palette = ppu.oam_attribute_regs[i] & 0x03;
            let sprite_addr = sprite_addr(sprite_palette, sprite_index as u16);
            let sprite_color_index = ppu.mapper.borrow().get_byte_ppu(sprite_addr);
            let sprite_color = ppu.ntsc_to_rgb(sprite_color_index);
            ppu.framebuffer[(cycle as usize)+(scanline as usize)*256] = sprite_color;
            return;
        }
    }
    
    let pixel_addr = pixel_addr(palette_index, bg_index);
    let pixel_color_index = ppu.mapper.borrow().get_byte_ppu(pixel_addr);
    let pixel_color = ppu.ntsc_to_rgb(pixel_color_index);
    
    //println!("Drawing cycle {} of scanline {}", cycle, scanline);
    if palette_index == 2 {
    //println!("BG index: {}, palette index: {}, pixel addr: {:#X}, pixel index: {:#X}, pixel color: {:#X}, shift: {}", bg_index, palette_index, pixel_addr, pixel_color_index, pixel_color, shift);
    }
    //println!("Rendering pixel at {}, color {:#X}", (cycle as usize)+(scanline as usize)*256, pixel_color);
    ppu.framebuffer[(cycle as usize)+(scanline as usize)*256] = pixel_color;
}

fn pixel_addr(palette_index: u8, bg_index: u16) -> u16 {
    if bg_index == 0 {
        0x3F00u16
    }
    else {
        0x3F00u16 + ((palette_index as u16) * 4) + (bg_index)
    }
}

fn sprite_addr(palette_index: u8, sprite_index: u16) -> u16 {
    if sprite_index == 0 {
        0x3F00u16
    }
    else {
        0x3F10u16 + ((palette_index as u16) * 4) + (sprite_index)
    }
}

impl From<BGOp> for fn(&mut PPU, i8, u16) {
    fn from(x: BGOp) -> fn(&mut PPU, i8, u16) {
        match x {
            BGOp::WaitMem => |_, _, _| {
            },
            BGOp::Nop => |ppu, _, scanline| {
                //println!("\t\tBG Nop, scanline {}, cycle {}", scanline, ppu.cycle_count);
                if scanline < 240 || scanline == 261 {
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchNT,
                        tile: 2,
                        scanline,
                    });
                }
                else {
                    // No pixels on these scanlines
                    ppu.queue.push_front(BGState {
                        op: BGOp::Nop,
                        tile: 2,
                        scanline: (scanline + 1) % 262,
                    });
                    if scanline == 241 {
                        ppu.queue.push_front(BGState {
                            op: BGOp::SetVBlank,
                            tile: 0, // this doesn't matter
                            scanline,
                        });
                        for i in (1..340).rev() {
                            ppu.queue.push_front(BGState {
                                op: BGOp::WaitMem,
                                tile: 0, // this doesn't matter
                                scanline,
                            });
                        }
                    } else {
                        for i in (1..341).rev() {
                            ppu.queue.push_front(BGState {
                                op: BGOp::WaitMem,
                                tile: 0, // this doesn't matter
                                scanline,
                            });
                        }
                    }
                }
            },
            BGOp::SetVBlank => |ppu, tile, scanline| {
                // set vblank
                ppu.ppustatus |= 0x80;
                // set nmi_occurred
                ppu.nmi_occurred = true;
            },
            BGOp::FetchNT => |ppu, tile, scanline| {
                if scanline == 261 && tile == 2 {
                    // clear vblank, sprite 0, overflow
                    ppu.ppustatus &= !0xE0;
                    // move out of reset, if applicable
                    ppu.initialized = true;
                    // clear nmi
                    ppu.nmi_occurred = false;
                }
                let base = ppu.get_nt_base();
                //let addr = base + (tile as u16) + ((scanline as u16)/ 8);
                let addr = base + (tile as u16) + (((scanline as u16)/ 8) * 32);
                //println!("Fetching NT byte at address {:#X}", addr);
                ppu.nametable_byte = ppu.mapper.borrow().get_byte_ppu(addr);
                //println!("NT == {:#X}, from address {:#X}, for tile {}", ppu.nametable_byte, addr, tile);
                //assert!(ppu.nametable_byte == 0x24 || ppu.nametable_byte == 0x00);
                if tile == -2 {
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchNT,
                        tile: tile+1,
                        scanline: scanline,
                    });
                    ppu.queue.push_front(BGState {
                        op: BGOp::WaitMem,
                        tile: tile,
                        scanline: scanline,
                    });
                }
                else if tile == -1 {
                    ppu.queue.push_front(BGState {
                        op: BGOp::Nop,
                        tile: 2,
                        scanline: scanline,
                    });
                    ppu.queue.push_front(BGState {
                        op: BGOp::WaitMem,
                        tile: tile,
                        scanline: scanline,
                    });
                }
                else {
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchAT,
                        tile: tile,
                        scanline: scanline,
                    });
                    ppu.queue.push_front(BGState {
                        op: BGOp::WaitMem,
                        tile: tile,
                        scanline: scanline,
                    });
                }
            },
            BGOp::FetchAT => |ppu, tile, scanline| {
                let base = 0x23C0u16;
                if scanline < 240 && tile < 32 {
                    //let addr = base + ((ppu.nametable_byte as u16) / 4);
                    //let addr = base + ((tile as u16)/4) + (scanline as u16) / 4;
                    let addr = base + (((scanline as u16) / 8 / 4)*8) + (tile as u16 / 4);
                    // if scanline % 16 < 8, bottom half of byte
                    // if tile % 2 == 0, low bits, else high bits
                    let tile_x = tile;
                    let tile_y = scanline / 8;
                    let bigshift = if tile_y % 4 < 2 { 0 } else { 4 };
                    let smallshift = if tile_x % 4 < 2 { 0 } else { 2 };
                    let loaded = ppu.mapper.borrow().get_byte_ppu(addr);
                    ppu.attribute_latch = (loaded >> (bigshift + smallshift)) & 0x03;
                    //println!("addr: {:#X} loaded: {:#X} scanline: {} tile: {} bigshift: {} smallshift: {} AT: {}", addr, loaded, scanline, tile, bigshift, smallshift, ppu.attribute_latch);
                    assert!(addr < 0x2400);
                }
                ppu.queue.push_front(BGState {
                    op: BGOp::FetchBGLow,
                    tile: tile,
                    scanline: scanline,
                });
                ppu.queue.push_front(BGState {
                    op: BGOp::WaitMem,
                    tile: tile,
                    scanline: scanline,
                });
            },
            BGOp::FetchBGLow => |ppu, tile, scanline| {
                let base = ppu.get_bg_base();
                // TODO maybe multiply by 8 here? not sure
                let addr = base + ((ppu.nametable_byte as u16) * 16) + scanline % 8;
                //println!("Accessing address {:#X} for BG Low, NT {:#X}, tile {}, scanline {}", addr, ppu.nametable_byte, tile, scanline);
                ppu.bg_low_byte = ppu.mapper.borrow().get_byte_ppu(addr);
                ppu.queue.push_front(BGState {
                    op: BGOp::FetchBGHigh,
                    tile: tile,
                    scanline: scanline,
                });
                ppu.queue.push_front(BGState {
                    op: BGOp::WaitMem,
                    tile: tile,
                    scanline: scanline,
                });
            },
            BGOp::FetchBGHigh => |ppu, tile, scanline| {
                let base = ppu.get_bg_base();
                let addr = base + ((ppu.nametable_byte as u16) * 16) + 8 + scanline % 8;
                //println!("Accessing address {:#X} for BG High", addr);
                ppu.bg_high_byte = ppu.mapper.borrow().get_byte_ppu(addr);
                ppu.queue.push_front(BGState {
                    op: BGOp::IncHoriz,
                    tile: tile,
                    scanline: scanline,
                });
            },
            BGOp::IncHoriz => |ppu, tile, scanline| {
                //assert!(ppu.bg_low_byte == 0x00 && ppu.bg_high_byte == 0x00);
                //println!("\t\tBG IncHoriz, tile {}, scanline {}, low {:#X}, high {:#X}", tile, scanline, ppu.bg_low_byte, ppu.bg_high_byte);
                //println!("Loading shift registers with pattern data");
                ppu.bg_pattern_latches[0] = ppu.bg_pattern_latches[0] >> 8;
                ppu.bg_pattern_latches[0] |= (ppu.bg_low_byte as u16) << 8;
                ppu.bg_pattern_latches[1] = ppu.bg_pattern_latches[1] >> 8;
                ppu.bg_pattern_latches[1] |= (ppu.bg_high_byte as u16) << 8;
                //println!("Loading shift registers with attribute data");
                ppu.bg_attribute_latches[0] = ppu.bg_attribute_latches[1];
                ppu.bg_attribute_latches[1] = ppu.attribute_latch;
                if tile == 33 {
                    // delay through HBLANK
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchNT,
                        tile: 0,
                        scanline: (scanline + 1) % 262,
                    });
                    for i in (31..39).rev() {
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+7, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+6, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+5, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+4, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+3, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+2, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i+1, // this doesn't matter
                            scanline,
                        });
                        ppu.queue.push_front(BGState {
                            op: BGOp::WaitMem,
                            tile: i, // this doesn't matter
                            scanline,
                        });
                    }
                }
                else if tile == 1 {
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchNT,
                        tile: -2,
                        scanline: scanline,
                    });
                }
                else {
                    ppu.queue.push_front(BGState {
                        op: BGOp::FetchNT,
                        tile: tile + 1,
                        scanline: scanline,
                    });
                }
            }
        }
    }
}

impl From<SpriteOp> for fn(&mut PPU, u16, u16) {
    fn from(x: SpriteOp) -> fn(&mut PPU, u16, u16) {
        match x {
            SpriteOp::WaitMem => |_, _, _| {},
            SpriteOp::Nop => |ppu, cycle, scanline| {
                println!("\tSprite Nop, scanline {}, cycle {}", scanline, ppu.cycle_count);
                if scanline < 240 || scanline == 261 {
                //if false {
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ClearOAM,
                        cycle: cycle + 1,
                        scanline,
                    });
                }
                else {
                    // No sprites on these scanlines
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::Nop,
                        cycle: 0,
                        scanline: scanline + 1,
                    });
                    for i in (1..341).rev() {
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::WaitMem,
                            cycle: i,
                            scanline,
                        });
                    }
                }
            },
            SpriteOp::ClearOAM => |ppu, cycle, scanline| {
                ppu.secondary_oam[((cycle as usize)-1)/2] = 0xFF;
                if cycle == 63 {
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::TestSprite,
                        cycle: cycle + 3,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 2,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WaitMem,
                        cycle: cycle + 1,
                        scanline,
                    });
                    ppu.sprite_index = 0;
                }
                else {
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ClearOAM,
                        cycle: cycle + 2,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WaitMem,
                        cycle: cycle + 1,
                        scanline,
                    });
                }
            },
            SpriteOp::ReadOAM => |ppu, cycle, scanline| {
                // starts at cycle 65
                ////println!("reading OAM index {}", ppu.sprite_index);
                if ppu.sprite_index > 255 {
                    //println!("sprite_index: {}, sprite_count: {}, sprite_offset: {}", ppu.sprite_index, ppu.sprite_count, ppu.sprite_offset);
                }
                ppu.oam_latch = ppu.oam[ppu.sprite_index as usize];
                ppu.sprite_index = ppu.sprite_index.wrapping_add(1);
                // reset oamaddr
                ppu.oamaddr = 0;
            },
            SpriteOp::TestSprite => |ppu, cycle, scanline| {
                //println!("TestSprite, cycle {}, scanline {}, sprite_index {}", cycle, scanline, ppu.sprite_index);
                // reset oamaddr
                ppu.oamaddr = 0;
                let y_coord = ppu.oam_latch as u16;
                if ppu.sprite_index > 252 || cycle >= 253 {
                    //println!("Done checking sprites, no overflow");
                    ppu.sprite_index = 0;
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::FetchNT,
                        cycle: 257,
                        scanline,
                    });
                    for i in (cycle+1..257).rev() {
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::WaitMem,
                            cycle: i,
                            scanline,
                        });
                    }
                }
                else if y_coord <= scanline + 1 && y_coord + ppu.get_sprite_height() > scanline + 1 {
                    //println!("Sprite hit, sprite_count {}", ppu.sprite_count);
                    println!("writing {} to offset {} in secondary OAM", ppu.oam_latch - 1, ppu.sprite_offset);
                    ppu.secondary_oam[ppu.sprite_count as usize] = ppu.oam_latch - 1;
                    ppu.sprite_offset += 1;
                    if ppu.sprite_count == 7 {
                        //println!("Sprite count 7, checking overflow");
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::CheckOverflow,
                            cycle: cycle + 8,
                            scanline,
                        });
                    }
                    else {
                        //println!("Sprite count !=7, continuing to check sprites");
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::TestSprite,
                            cycle: cycle + 8,
                            scanline,
                        });
                    }
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 7,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WriteOAM,
                        cycle: cycle + 6,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 5,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WriteOAM,
                        cycle: cycle + 4,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 3,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WriteOAM,
                        cycle: cycle + 2,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 1,
                        scanline,
                    });
                }
                else {
                    //println!("No sprite hit");
                    ppu.sprite_index += 3;
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::TestSprite,
                        cycle: cycle + 2,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAM,
                        cycle: cycle + 1,
                        scanline,
                    });
                }
            },
            SpriteOp::WriteOAM => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                let offset = ppu.sprite_offset as usize;
                println!("writing {} to offset {} in secondary OAM, sprite {}", ppu.oam_latch, offset, ppu.sprite_count);
                ppu.secondary_oam[((ppu.sprite_count as usize) * 4) + offset] = ppu.oam_latch;
                ppu.sprite_offset += 1;
                if ppu.sprite_offset == 4 {
                    ppu.sprite_count += 1;
                    ppu.sprite_offset = 0;
                }
            },
            SpriteOp::CheckOverflow => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                let y_coord = ppu.oam_latch as u16;
                if y_coord <= scanline + 1 && y_coord + ppu.get_sprite_height() > scanline + 1 {
                    // overflow has occurred - set flag
                    ppu.ppuctrl |= 0x20;
                    ppu.sprite_index = 0;
                    // read next 3 bytes (no copy) and delay until HBLANk
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::FetchNT,
                        cycle: 257,
                        scanline,
                    });
                    // delay until HBLANK
                        //println!("DELAY UNTIL HBLANK");
                    for i in (cycle+6..257).into_iter().rev() {
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::WaitMem,
                            cycle: i,
                            scanline,
                        });
                    }
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAMBugged,
                        cycle: cycle + 5,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WaitMem,
                        cycle: cycle + 4,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAMBugged,
                        cycle: cycle + 3,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::WaitMem,
                        cycle: cycle + 2,
                        scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::ReadOAMBugged,
                        cycle: cycle + 1,
                        scanline,
                    });
                }
                else {
                    ppu.sprite_index += 5; // this is the bug
                    if ppu.sprite_index > 255 {
                        panic!();
                        ppu.sprite_index = 0;
                        // read next 3 bytes (no copy) and delay until HBLANk
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::FetchNT,
                            cycle: 257,
                            scanline,
                        });
                        // delay until HBLANK
                        for i in (cycle+1..257).rev() {
                            ppu.sprite_queue.push_front(SpriteState {
                                op: SpriteOp::WaitMem,
                                cycle: i,
                                scanline,
                            });
                        }
                    }
                    else {
                        // read again then run this check again
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::CheckOverflow,
                            cycle: cycle + 2,
                            scanline,
                        });
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::ReadOAMBugged,
                            cycle: cycle + 1,
                            scanline,
                        });
                    }
                }
            },
            SpriteOp::ReadOAMBugged => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                ////println!("reading OAM index {} (buggy)", ppu.sprite_index);
                ppu.oam_latch = ppu.oam[ppu.sprite_index as usize];
            },
            SpriteOp::FetchNT => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                ////println!("fetching sprite NT on cycle {}", cycle);
                let base = ppu.get_nt_base();
                let addr = base as u16;
                let _ = ppu.mapper.borrow().get_byte_ppu(addr);
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::FetchAT,
                    cycle: cycle + 2,
                    scanline: scanline,
                });
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::WaitMem,
                    cycle: cycle + 1,
                    scanline: scanline,
                });
            },
            SpriteOp::FetchAT => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                ////println!("fetching sprite AT on cycle {}", cycle);
                let base = 0x23C0u16;
                let addr = base as u16;
                let _ = ppu.mapper.borrow().get_byte_ppu(addr);
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::FetchSpriteLow,
                    cycle: cycle + 2,
                    scanline: scanline,
                });
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::WaitMem,
                    cycle: cycle + 1,
                    scanline: scanline,
                });
            },
            SpriteOp::FetchSpriteLow => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                ////println!("fetching sprite low on cycle {}", cycle);
                let sprite_index = ppu.sprite_index as usize;
                let y_start = ppu.secondary_oam[sprite_index*4];
                if y_start < 0xEF {
                    let tile_index = (ppu.secondary_oam[sprite_index*4+1] as u16);
                    let y_offset = (scanline) - (ppu.secondary_oam[sprite_index*4] as u16);
                    let addr = ppu.get_sprite_base() + ((tile_index as u16) * 16) + y_offset;
                    ////println!("accessing byte {} from secondary OAM", (ppu.sprite_index as usize * 4) + 1);
                    println!("sprite {}, scanline {}, cycle {},  y_start {}, y_offset {}, tile_index {}", sprite_index, scanline, cycle, ppu.secondary_oam[sprite_index*4], y_offset, tile_index);
                    ppu.sprite_low_byte = ppu.mapper.borrow().get_byte_ppu(addr);
                }
                else {
                    // transparent
                    ppu.sprite_low_byte = 0;
                }
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::FetchSpriteHigh,
                    cycle: cycle + 2,
                    scanline: scanline,
                });
                ppu.sprite_queue.push_front(SpriteState {
                    op: SpriteOp::WaitMem,
                    cycle: cycle + 1,
                    scanline: scanline,
                });
            },
            SpriteOp::FetchSpriteHigh => |ppu, cycle, scanline| {
                // reset oamaddr
                ppu.oamaddr = 0;
                ////println!("fetching sprite high on cycle {}", cycle);
                let sprite_index = ppu.sprite_index as usize;
                let y_start = ppu.secondary_oam[sprite_index*4];
                if y_start < 0xEF {
                    let tile_index = (ppu.secondary_oam[sprite_index*4+1] as u16);
                    let y_offset = (scanline) - (ppu.secondary_oam[sprite_index*4] as u16);
                    let addr = ppu.get_sprite_base() + ((tile_index as u16) * 16) + y_offset + 8;
                    ////println!("accessing byte {} from secondary OAM", (ppu.sprite_index as usize * 4) + 1);
                    ppu.sprite_high_byte = ppu.mapper.borrow().get_byte_ppu(addr);
                }
                else {
                    // transparent
                    ppu.sprite_high_byte = 0;
                }
                if ppu.sprite_index >= 7 {
                    assert_eq!(cycle, 319);
                    ppu.sprite_index = 0;
                    ppu.sprite_count = 0;
                    // wait for end of scanline
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::Nop,
                        cycle: 0,
                        scanline: ((scanline+1) % 262),
                    });
                    for i in (cycle+1..341).rev() {
                        ppu.sprite_queue.push_front(SpriteState {
                            op: SpriteOp::WaitMem,
                            cycle: i,
                            scanline: scanline,
                        });
                    }
                }
                else {
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::FetchNT,
                        cycle: cycle + 2,
                        scanline: scanline,
                    });
                    ppu.sprite_queue.push_front(SpriteState {
                        op: SpriteOp::IncHoriz,
                        cycle: cycle + 1,
                        scanline: scanline,
                    });
                }
            },
            SpriteOp::IncHoriz => |ppu, cycle, scanline| {
                let sprite_index = ppu.sprite_index as usize;
                ppu.oam_pattern_regs[sprite_index][0] = ppu.sprite_low_byte;
                ppu.oam_pattern_regs[sprite_index][1] = ppu.sprite_high_byte;
                ppu.oam_attribute_regs[sprite_index] = ppu.secondary_oam[sprite_index*4+2];;
                ppu.oam_xpos_regs[sprite_index] = ppu.secondary_oam[sprite_index*4+3];
                ppu.sprite_index += 1;
                //println!("sprite_index is now {}", ppu.sprite_index);
            },
        }
    }
}

pub struct PPU {
    ppuctrl: u8,
    ppumask: u8,
    ppustatus: u8,
    oamaddr: u8,
    latch_cleared: Cell<bool>,
    latch_low: u8,
    latch_high: u8,
    ppuscroll: u8,
    ppuaddr: u8,
    ppudata: u8,
    queue: VecDeque<BGState>,
    mapper: Rc<RefCell<dyn Mapper>>,
    sprite_queue: VecDeque<SpriteState>,
    oam: [u8; 256],
    secondary_oam: [u8; 32],
    oam_pattern_regs: [[u8; 2]; 8],
    oam_attribute_regs: [u8; 8],
    oam_xpos_regs: [u8; 8],
    
    initialized: bool,
    nametable_byte: u8,
    attribute_latch: u8,
    bg_low_byte: u8,
    bg_high_byte: u8,
    
    oam_latch: u8,
    sprite_count: u8,
    sprite_index: u8,
    sprite_offset: u8,
    sprite_low_byte: u8,
    sprite_high_byte: u8,
    
    cycle_count: usize,
    
    bg_pattern_latches: [u16; 2],
    bg_attribute_latches: [u8; 2],
    
    pub framebuffer: Vec<u32>,
    
    ntsc_lookup: [u8; 256],
    
    nmi_occurred: bool,
}

impl PPU {
    fn get_sprite_height(&self) -> u16 {
        let index = self.ppuctrl >> 5 & 0x01;
        if index > 0 {
            16
        }
        else {
            8
        }
    }
    fn get_nt_base(&self) -> u16 {
        let index = self.ppuctrl & 0x03;
        match index {
            0 => 0x2000u16,
            1 => 0x2400u16,
            2 => 0x2800u16,
            3 => 0x2C00u16,
            _ => unreachable!(),
        }
    }
    
    fn get_bg_base(&self) -> u16 {
        let index = self.ppuctrl >> 4 & 0x01;
        match index {
            0 => 0x0000u16,
            1 => 0x1000u16,
            _ => unreachable!(),
        }
    }
    
    fn get_sprite_base(&self) -> u16 {
        let index = self.ppuctrl >> 3 & 0x05;
        match index {
            0b000 => 0x0000u16,
            0b100 => 0x0000u16,
            0b101 => 0x0000u16,
            0b001 => 0x1000u16,
            _ => unreachable!(),
        }
    }
    
    pub fn get_reg(&self, addr: u16) -> &u8 {
        assert!(addr >= 0x2000u16 && addr < 0x2008u16);
        
        let index = addr - 0x2000;
        match index {
            0 => &self.ppuctrl,
            1 => &self.ppumask,
            2 => {
                self.latch_cleared.set(true);
                &self.ppustatus
            },
            3 => &self.oamaddr,
            4 => unimplemented!(),
            5 => &self.ppuscroll,
            6 => &self.ppuaddr,
            7 => &self.ppudata,
            _ => unreachable!(),
        }
    }
    
    pub fn get_reg_mut(&mut self, addr: u16) -> &mut u8 {
        assert!(addr >= 0x2000u16 && addr < 0x2008u16);
        
        let index = addr - 0x2000;
        match index {
            0 => &mut self.ppuctrl,
            1 => &mut self.ppumask,
            2 => {
                self.latch_cleared.set(true);
                &mut self.ppustatus
            },
            3 => &mut self.oamaddr,
            4 => unimplemented!(),
            5 => &mut self.ppuscroll,
            6 => &mut self.ppuaddr,
            7 => &mut self.ppudata,
            _ => unreachable!(),
        }
    }
    
    pub fn set_reg(&mut self, addr: u16, value: u8) {
        assert!(addr >= 0x2000u16 && addr < 0x2008u16);
        
        if !self.initialized {
            // ignore write while in reset
            return;
        }
        
        let index = addr - 0x2000;
        match index {
            0 => {
                assert!(value & 0x40 == 0, "are you _trying_ to blow up your NES?");
                self.ppuctrl = value;
            },
            1 => self.ppumask = value,
            2 => unimplemented!(),
            3 => self.oamaddr = value,
            4 => unimplemented!(),
            5 => {
                //println!("Setting ppuscroll to {:#X}", value);
                self.ppuscroll = value;
            },
            6 => {
                if self.latch_cleared.get() {
                    //println!("Setting high latch value to {:#X}", value);
                    self.latch_high = value;
                    self.latch_cleared.set(false);
                }
                else {
                    //println!("Setting low latch value to {:#X}", value);
                    self.latch_low = value;
                    self.latch_cleared.set(true);
                }
                //println!("Write to PPUADDR. New latch value is {:#X}", self.get_latch());
            },
            7 => {
                if self.get_latch() == 0x2350 {
                    //println!("Write {:#X} to PPUDATA at {:#X}", value, self.get_latch());
                    if value != 0x24 && value != 0x12 {
                        //println!("Current PPU cycle: {}", self.cycle_count);
                        //panic!();
                    }
                }
                self.mapper.borrow_mut().set_byte_ppu(self.get_latch(), value);
                let increment = if self.ppuctrl & 0x04 > 0 {
                    32
                } else {
                    1
                };
                self.set_latch(self.get_latch() + increment);
            },
            _ => unreachable!(),
        }
    }
    
    fn get_latch(&self) -> u16 {
        (self.latch_high as u16) << 8 | self.latch_low as u16
    }
    
    fn set_latch(&mut self, value: u16) {
        self.latch_high = (value >> 8) as u8;
        self.latch_low = value as u8;
    }
    
    pub fn with_mapper(mapper: Rc<RefCell<dyn Mapper>>) -> PPU {
        let mut queue = VecDeque::new();
        queue.push_front(BGState {
            op: BGOp::Nop,
            tile: 2,
            scanline: 261,
        });
        let mut sprite_queue = VecDeque::new();
        sprite_queue.push_front(SpriteState {
            op: SpriteOp::Nop,
            cycle: 0,
            scanline: 261,
        });
        let mut framebuffer = Vec::with_capacity(61440);
        framebuffer.resize_with(61440, || 0);
        PPU {
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            latch_cleared: Cell::new(true),
            latch_low: 0,
            latch_high: 0,
            ppuscroll: 0,
            ppuaddr: 0,
            ppudata: 0,
            queue,
            mapper,
            sprite_queue,
            oam: [0; 256],
            secondary_oam: [0; 32],
            initialized: false,
            oam_pattern_regs: [[0; 2]; 8],
            oam_attribute_regs: [0; 8],
            oam_xpos_regs: [0; 8],
            nametable_byte: 0,
            attribute_latch: 0,
            bg_low_byte: 0,
            bg_high_byte: 0,
            oam_latch: 0,
            sprite_count: 0,
            sprite_index: 0,
            sprite_offset: 0,
            sprite_low_byte: 0,
            sprite_high_byte: 0,
            cycle_count: 0,
            bg_pattern_latches: [0; 2],
            bg_attribute_latches: [0; 2],
            framebuffer,
            ntsc_lookup,
            nmi_occurred: false,
        }
    }
    
    fn ntsc_to_rgb(&self, ntsc_byte: u8) -> u32 {
        let mut color = 0u32;
        for i in 0..4 {
            // color in RGBA format
            color |= (self.ntsc_lookup[((ntsc_byte * 4) + i) as usize] as u32) << (i*8);
            //println!("Color now {:#X}", color);
        }
        color
    }
    
    pub fn write_oam(&mut self, addr: u8, value: u8) {
        self.oam[addr as usize] = value;
    }
    
    pub fn run(&mut self) -> bool {
        let mut nmi = false;
        for i in 0..3 {
            //println!("Executing PPU cycle {}", self.cycle_count);
            //println!("Executing PPU cycle {}, sprite_index == {}", self.cycle_count, self.sprite_index);
            let state = self.queue.pop_front().unwrap();
            let uop = state.op;
            let sprite_state = self.sprite_queue.pop_front().unwrap();
            let sprite_uop = sprite_state.op;
            
            if sprite_uop == SpriteOp::Nop {
                if uop != BGOp::Nop || state.scanline != sprite_state.scanline {
                    println!("Failure on PPU cycle {}", self.cycle_count);
                    //panic!();
                }
            }
            //println!("\tDispatching BG uop {:?}", uop);
            let ufn :fn(&mut PPU, i8, u16) = uop.into();
            (ufn)(self, state.tile, state.scanline);
            drawy_bits(self, sprite_state.cycle, sprite_state.scanline);
            
            
            
            //println!("\tDispatching sprite uop {:?}", sprite_uop);
            let ufn :fn(&mut PPU, u16, u16) = sprite_uop.into();
            (ufn)(self, sprite_state.cycle, sprite_state.scanline);
            self.cycle_count += 1;
            
            if self.nmi_occurred && self.ppuctrl & 0x80 > 0 {
                // an nmi has occurred
                nmi = true;
            }
            
            //if self.cycle_count > 500_000 {
            //    for i in 0x23C0..0x2400 {
            //        println!("AT {:#X}: {:#X}.", 
            //            i,
            //            self.mapper.borrow().get_byte_ppu(i));
            //    }
            //    panic!();
            //}
        }
        nmi
    }
}
