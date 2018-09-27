#![feature(nll)]
#![feature(external_doc)]
#![warn(missing_docs)]
#![doc(include = "../README.md")]

#[macro_use]
extern crate gfx;
extern crate alga;
extern crate crossbeam;
extern crate fnv;
extern crate gfx_window_glutin;
extern crate gifski;
extern crate glutin;
extern crate image;
extern crate itertools;
pub extern crate nalgebra;
pub extern crate palette;
extern crate rand;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate tempfile;

use fnv::FnvHashMap as HashMap;
use gfx::format::{Formatted, SurfaceTyped};
use gfx::memory::Typed;
use gfx::traits::FactoryExt;
use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::dpi::LogicalSize;
use glutin::Api::OpenGl;
use glutin::{GlContext, GlRequest};
use nalgebra::{Matrix3, Vector2};
use palette::{Srgb, Srgba};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;

mod render;
pub mod state;

pub use glutin::VirtualKeyCode as Key;
pub use state::{Fill, PollockState, Stroke};
pub use std::f64::consts;

use render::DrawState;
use state::ExtendedState;

type ColorFormat = gfx::format::Srgba8;
type DepthFormat = gfx::format::DepthStencil;
type SurfaceData = <<ColorFormat as Formatted>::Surface as SurfaceTyped>::DataType;

pub type V2 = Vector2<f64>;
pub type Transform = Matrix3<f64>;
pub type Color = Srgba<u8>;

pub fn v2<X: Into<f64>, Y: Into<f64>>(x: X, y: Y) -> V2 {
    V2::new(x.into(), y.into())
}

pub fn rgb(r: u8, g: u8, b: u8) -> Color {
    Srgb::new(r, g, b).into()
}

pub fn rgba(r: u8, g: u8, b: u8, a: u8) -> Color {
    Srgba::new(r, g, b, a)
}

#[macro_export]
macro_rules! state {
    ($($name:ident : $t:ty = $val:expr),*$(,)*) => {{
        struct State {
            $($name : $t),*
        }

        State {
            $($name: $val),*
        }
    }};
}

struct KeyHandler<'a, State> {
    down: Option<Box<FnMut(&mut PollockState<State>) + 'a>>,
    up: Option<Box<FnMut(&mut PollockState<State>) + 'a>>,
}

/// Entry point for `Pollock`. The entrypoint for making a new Pollock project is `setup()`.
///
/// To create a new Pollock instance, you call `setup` with a setup function that will be
/// executed when your application is started.
///
/// ```rust
/// // This will emit a warning saying that you must call `run`
/// Pollock::setup(|p| {
///     // Do your setup here
/// });
/// ```
///
/// To start a Pollock program, you call `run` on the resultant object. This will go
/// into a loop that will only return when the window is closed, so make sure you've set up
/// everything that your program needs before you call it.
///
/// ```no_run
/// Pollock::setup(|p| {
///     // Setup code
/// }).run();
/// ```
///
/// To actually _do_ anything with Pollock, you need to add code to `draw`. Unlike Processing,
/// which allows you to call functions that draw to the screen at any time, Pollock only allows
/// you to draw in the draw function itself. You can draw to the screen with `p.circle`, `p.rect`
/// and so forth. For a full list of functions see the documentation for `ExtendedState`.
///
/// ```no_run
/// let size = (600, 600);
/// Pollock::setup(|p| {
///     p.size = size;
/// }).draw(|p| {
///     // Example spinning square
///     let mut p = p.push();
///     p.rotate(p.frame_count as f64 / 100.);
///     p.translate(v2(size.0, size.1));
///     p.square(v2(0, 0), 50);
/// }).run();
/// ```
#[must_use = "Pollock does nothing until you call `.run()`"]
pub struct Pollock<'a, State, SetupFn, DrawFn> {
    setup_fn: Option<SetupFn>,
    draw_fn: DrawFn,
    key_handlers: HashMap<Key, KeyHandler<'a, State>>,
    frame_handlers: Vec<(usize, Box<FnMut(&mut PollockState<State>) + 'a>)>,
}

impl<'a, S, SFn> Pollock<'a, S, SFn, fn(&mut ExtendedState<&mut PollockState<S>, DrawState>)> {
    pub fn setup(
        fun: SFn,
    ) -> Pollock<'a, S, SFn, fn(&mut ExtendedState<&mut PollockState<S>, DrawState>)>
    where
        SFn: FnOnce(&mut PollockState<()>) -> S,
    {
        Pollock {
            setup_fn: Some(fun),
            key_handlers: Default::default(),
            frame_handlers: Default::default(),
            draw_fn: |_| {},
        }
    }
}

impl<'a, S, SFn, DFn> Pollock<'a, S, SFn, DFn>
where
    SFn: FnOnce(&mut PollockState<()>) -> S,
    DFn: FnMut(&mut ExtendedState<&mut PollockState<S>, DrawState>),
    // TODO: Remove this bound if we're not hot reloading
    S: Serialize,
    for<'any> S: Deserialize<'any>,
{
    pub fn draw<F: FnMut(&mut ExtendedState<&mut PollockState<S>, DrawState>) + 'a>(
        self,
        fun: F,
    ) -> Pollock<'a, S, SFn, F> {
        Pollock {
            setup_fn: self.setup_fn,
            key_handlers: self.key_handlers,
            frame_handlers: self.frame_handlers,
            draw_fn: fun,
        }
    }

    // TODO: Do we need this?
    pub fn on_frame<F: FnMut(&mut PollockState<S>) + 'a>(mut self, frame: usize, fun: F) -> Self {
        match self
            .frame_handlers
            .binary_search_by_key(&frame, |handler| handler.0)
        {
            Ok(_) => unimplemented!("We don't handle 2 handlers for the same frame yet"),
            Err(pos) => self.frame_handlers.insert(pos, (frame, Box::new(fun))),
        }

        self
    }

    pub fn on_key_down<F: FnMut(&mut PollockState<S>) + 'a>(mut self, key: Key, fun: F) -> Self {
        use std::collections::hash_map::Entry;

        let boxed = Box::new(fun);

        // TODO: Handle calling this multiple times with the same key somehow
        match self.key_handlers.entry(key) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().down = Some(boxed);
            }
            Entry::Vacant(entry) => {
                entry.insert(KeyHandler {
                    down: Some(boxed),
                    up: None,
                });
            }
        }

        self
    }

    pub fn on_key_up<F: FnMut(&mut PollockState<S>) + 'a>(mut self, key: Key, fun: F) -> Self {
        use std::collections::hash_map::Entry;

        let boxed = Box::new(fun);

        // TODO: Handle calling this multiple times with the same key somehow
        match self.key_handlers.entry(key) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().up = Some(boxed);
            }
            Entry::Vacant(entry) => {
                entry.insert(KeyHandler {
                    up: Some(boxed),
                    down: None,
                });
            }
        }

        self
    }

    pub fn run(mut self) {
        use gfx::Factory;
        use render::{pipe, Transform};
        use std::{env, fs, mem};

        let mut state: PollockState<S> = if let Ok(statefile) = env::var("POLLOCK_STATE") {
            // TODO: Handle this more gracefully
            let file =
                fs::File::open(&statefile).expect("Hot reload failed - state file non-existant");
            serde_json::from_reader(&file).expect("Hot reload failed - state file out of date")
        } else {
            let mut init_state = PollockState::new((), state::InternalState::default());

            let inner_state = (self.setup_fn.take().unwrap())(&mut init_state);

            init_state.with_state(inner_state)
        };

        let mut events_loop = glutin::EventsLoop::new();
        let windowbuilder = glutin::WindowBuilder::new()
            .with_title("Pollock")
            .with_dimensions(LogicalSize::from_physical(state.size, 2.0));
        let contextbuilder = glutin::ContextBuilder::new()
            .with_gl(GlRequest::Specific(OpenGl, (3, 2)))
            .with_vsync(true);

        let (window, ..) = gfx_glutin::init::<ColorFormat, DepthFormat>(
            windowbuilder,
            contextbuilder,
            &events_loop,
        );

        let (mut cached_vertices, mut cached_indices) = (vec![], vec![]);

        // We use this so that we can recreate the whole context after an iteration of
        // the inner loop in which the window was resized (or something else that requires
        // us to get the context again).
        let mut run_application = true;
        while run_application {
            let (mut device, mut factory, color_view, _depth_view) =
                gfx_glutin::init_existing::<ColorFormat, DepthFormat>(&window);

            let mut encoder: gfx::Encoder<_, _> = factory.create_command_buffer().into();
            let shaders = factory
                .create_shader_set(
                    render::shaders::VERTEX_SHADER.as_bytes(),
                    render::shaders::FRAGMENT_SHADER.as_bytes(),
                ).unwrap();
            let pso = factory
                .create_pipeline_state(
                    &shaders,
                    gfx::Primitive::TriangleList,
                    gfx::state::Rasterizer {
                        samples: Some(gfx::state::MultiSample),
                        ..gfx::state::Rasterizer::new_fill()
                    },
                    pipe::new(),
                ).unwrap();

            let mut run_context = true;
            let mut rendered = false;

            while run_application && run_context {
                // We do this at the start so that the window size is refreshed
                if state.size_dirty() {
                    let size = LogicalSize::from_physical(state.size, 2.0);
                    window.set_min_dimensions(Some(size));
                    window.set_max_dimensions(Some(size));
                    window.set_inner_size(size);

                    state.refresh_size();
                }

                events_loop.poll_events(|event| {
                    if let glutin::Event::WindowEvent { event, .. } = event {
                        match event {
                            glutin::WindowEvent::CloseRequested
                            | glutin::WindowEvent::KeyboardInput {
                                input:
                                    glutin::KeyboardInput {
                                        virtual_keycode: Some(glutin::VirtualKeyCode::Escape),
                                        ..
                                    },
                                ..
                            } => run_application = false,
                            glutin::WindowEvent::KeyboardInput { input, .. } => {
                                use glutin::ElementState;

                                if let Some(key) = input.virtual_keycode {
                                    match input.state {
                                        ElementState::Pressed => {
                                            state.keys.press(key);
                                            if let Some(handler) = self
                                                .key_handlers
                                                .get_mut(&key)
                                                .and_then(|handler| handler.down.as_mut())
                                            {
                                                handler(&mut state);
                                            }
                                        }
                                        ElementState::Released => {
                                            state.keys.release(key);
                                            if let Some(handler) = self
                                                .key_handlers
                                                .get_mut(&key)
                                                .and_then(|handler| handler.up.as_mut())
                                            {
                                                handler(&mut state);
                                            }
                                        }
                                    }
                                }
                            }
                            glutin::WindowEvent::Refresh => run_context = !rendered,
                            _ => {}
                        }
                    }
                });

                if rendered && state.paused {
                    continue;
                }

                match self
                    .frame_handlers
                    .binary_search_by_key(&state.frame_count, |handler| handler.0)
                {
                    Ok(elem) => (&mut self.frame_handlers[elem]).1(&mut state),
                    _ => {}
                }

                let (w, h) = (state.size.0 as f32, state.size.1 as f32);

                let largest_side = w.max(h);

                let transform = Transform {
                    transform: [
                        [2. / w, 0.0, 0.0, -1.],
                        // We do `-scale` here so that our coordinate system starts from the top-left
                        // instead of the bottom-left.
                        [0.0, -2. / h, 0.0, 1.],
                        [0.0, 0.0, 1.0, 0.0],
                        [0.0, 0.0, 0.0, 1.0],
                    ],
                };

                if !state.paused {
                    cached_vertices.clear();
                    cached_indices.clear();

                    // We cache the allocation for the vertices since we assume that it's rare that we will
                    // reduce the number of vertices in a scene significantly over the course of a run.
                    // If this assumption is violated we can shrink using heuristics like the average vec
                    // size over the past N frames or whatever.
                    let draw_state = RefCell::new(DrawState::new(
                        mem::replace(&mut cached_vertices, vec![]),
                        mem::replace(&mut cached_indices, vec![]),
                    ));

                    let mut ex_state = state.extend(&draw_state);
                    (self.draw_fn)(&mut ex_state);

                    let inner = &mut *ex_state.inner.borrow_mut();
                    cached_vertices = mem::replace(&mut inner.vertices, vec![]);
                    cached_indices = mem::replace(&mut inner.indices, vec![]);

                    state.frame_count += 1;
                }

                let (vertex_buffer, slice) = {
                    factory
                        .create_vertex_buffer_with_slice(&cached_vertices[..], &cached_indices[..])
                };

                let transform_buffer = factory.create_constant_buffer(1);

                let data = pipe::Data {
                    vbuf: vertex_buffer.clone(),
                    transform: transform_buffer.clone(),
                    out: color_view.clone(),
                };

                encoder.clear(
                    &color_view,
                    [
                        state.background.color.red as f32 / 255.,
                        state.background.color.green as f32 / 255.,
                        state.background.color.blue as f32 / 255.,
                        state.background.alpha as f32 / 255.,
                    ],
                );

                encoder
                    .update_buffer(&data.transform, &[transform], 0)
                    .unwrap();
                encoder.draw(&slice, &pso, &data);

                let (w, h, _, _) = data.out.get_dimensions();

                // Take screenshot/record gif
                // TODO: Just do a filesystem copy instead of taking the screenshot twice
                for path in state.save_paths() {
                    let download = factory
                        .create_download_buffer::<SurfaceData>(w as usize * h as usize)
                        .unwrap();

                    encoder
                        .copy_texture_to_buffer_raw(
                            data.out.raw().get_texture(),
                            None,
                            gfx::texture::RawImageInfo {
                                xoffset: 0,
                                yoffset: 0,
                                zoffset: 0,
                                width: w,
                                height: h,
                                depth: 0,
                                format: ColorFormat::get_format(),
                                mipmap: 0,
                            },
                            download.raw(),
                            0,
                        ).unwrap();

                    encoder.flush(&mut device);

                    let reader = factory.read_mapping(&download).unwrap();
                    // intermediary buffer only to avoid casting
                    let mut data = Vec::with_capacity(w as usize * h as usize * 4);

                    for pixel in reader.iter() {
                        data.extend(pixel);
                    }

                    let path: &std::path::Path = path.as_ref();
                    let path_with_ext = if path.extension().is_some() {
                        path.to_owned()
                    } else {
                        path.with_extension("png")
                    };

                    image::save_buffer(
                        &path_with_ext,
                        &data,
                        w as u32,
                        h as u32,
                        image::ColorType::RGBA(8),
                    ).unwrap();
                }

                encoder.flush(&mut device);

                window.swap_buffers().unwrap();
                device.cleanup();

                rendered = true;
            }
        }
    }
}
