use std::borrow::Cow;
use std::error::Error;
use std::io::Cursor;
use std::rc::Rc;

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
        data: &Vec<u8>,
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
                format: ClientFormat::U8U8U8,
            };
            let gl_texture = Texture2d::new(gl_ctx, raw)?;
            let texture_id = textures.insert(Rc::new(gl_texture));

            self.my_texture_id = Some(texture_id);
        }

        Ok(())
    }

    fn show_textures(&self, ui: &Ui, renderer: &mut Renderer, data: &mut Vec<u8>) {
        renderer.textures();
        Window::new(im_str!("Hello textures"))
            .size([640.0, 480.0], Condition::FirstUseEver)
            .build(ui, || {
                ui.text(im_str!("Hello textures!"));
                
                if let Some(my_texture_id) = self.my_texture_id {
                    let mouse_pos = ui.io().mouse_pos;
                    data.clear();
                    for i in 0..WIDTH {
                        for j in 0..HEIGHT {
                            // Insert RGB values
                            data.push(mouse_pos[0] as u8);
                            data.push(mouse_pos[1] as u8);
                            data.push((mouse_pos[0] + mouse_pos[1]) as u8);
                        }
                    }
                    let texture = renderer.textures().get(my_texture_id).unwrap();
                    let raw = RawImage2d {
                        data: Cow::Borrowed(data),
                        width: WIDTH as u32,
                        height: HEIGHT as u32,
                        format: ClientFormat::U8U8U8,
                    };
                    texture.write(glium::Rect { left: 0, bottom: 0, width: WIDTH as u32, height: HEIGHT as u32 }, raw);
                    ui.text("Some generated texture");
                    Image::new(my_texture_id, [WIDTH as f32, HEIGHT as f32]).build(ui);
                }
            });
    }
}

fn main() {
    let mut my_app = CustomTexturesApp::default();

    let mut data = Vec::with_capacity(WIDTH * HEIGHT);
    // Generate dummy texture
    for i in 0..WIDTH {
        for j in 0..HEIGHT {
            // Insert RGB values
            data.push((i/WIDTH) as u8);
            data.push((j/HEIGHT) as u8);
            data.push((i + j) as u8);
        }
    }
            
    let mut system = support::init(file!());
    my_app
        .register_textures(system.display.get_context(), &data, system.renderer.textures())
        .expect("Failed to register textures");
    system.main_loop(|_, ui, renderer| my_app.show_textures(ui, renderer, &mut data));
}
