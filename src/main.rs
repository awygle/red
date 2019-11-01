use std::borrow::Cow;
use std::error::Error;
use std::io::Cursor;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::{read, OpenOptions};
use red::ines::*;
use red::nrom_mapper::*;
use red::cpu;
#[macro_use]
extern crate slog;
use slog::Drain;

use glium::{
    backend::Facade,
    texture::{ClientFormat, RawImage2d},
    Texture2d,
};
use imgui::*;
use imgui_glium_renderer::*;

mod support;

const WIDTH: usize = 256;
const HEIGHT: usize = 240;


#[derive(Default)]
struct CustomTexturesApp {
    my_texture_id: Option<TextureId>,
}

impl CustomTexturesApp {
    fn register_textures<F>(
        &mut self,
        gl_ctx: &F,
        data: &[u32],
        textures: &mut Textures<Rc<Texture2d>>,
    ) -> Result<(), Box<dyn Error>>
    where
        F: Facade,
    {
        if self.my_texture_id.is_none() {

            let raw = RawImage2d {
                data: Cow::Borrowed(data),
                width: WIDTH as u32,
                height: HEIGHT as u32,
                format: ClientFormat::U32,
            };
            let gl_texture = Texture2d::new(gl_ctx, raw)?;
            let texture_id = textures.insert(Rc::new(gl_texture));

            self.my_texture_id = Some(texture_id);
        }

        Ok(())
    }

    fn show_textures(&self, ui: &Ui, renderer: &mut Renderer, cpu: &mut cpu::CPU) {
        renderer.textures();
        ui.window(im_str!("Hello textures"))
            .size([640.0, 480.0], Condition::FirstUseEver)
            .build(ui, || {
                ui.text(im_str!("Hello textures!"));
                
                if let Some(my_texture_id) = self.my_texture_id {
                    let texture = renderer.textures().get(my_texture_id).unwrap();
                    let raw = RawImage2d {
                        data: Cow::Owned(cpu.get_framebuffer().into()),
                        width: WIDTH as u32,
                        height: HEIGHT as u32,
                        format: ClientFormat::U32,
                    };
                    texture.write(glium::Rect { left: 0, bottom: 0, width: WIDTH as u32, height: HEIGHT as u32 }, raw);
                    ui.text("Some generated texture");
                    Image::new(my_texture_id, [WIDTH as f32, HEIGHT as f32]).build(ui);
                }
            });
        cpu.execute();
    }
}

fn main() {
    let mut my_app = CustomTexturesApp::default();
            
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
    let calc_pc = (mapper.get_byte(0xFFFD) as u16) << 8 | (mapper.get_byte(0xFFFC) as u16);
    let mut cpu = cpu::CPU::with_mapper(Rc::new(RefCell::new(mapper)), log);
    
    println!("calculated reset vector: {:#X}", calc_pc);
    println!("actual reset vector: {:#X}", cpu.get_pc());
    
    let mut system = support::init(file!());
    my_app
        .register_textures(system.display.get_context(), cpu.get_framebuffer(), system.renderer.textures())
        .expect("Failed to register textures");
    system.main_loop(|_, ui, renderer, cpu| my_app.show_textures(ui, renderer, cpu), cpu);
}
