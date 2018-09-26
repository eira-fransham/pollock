#![feature(nll)]

#[macro_use]
extern crate gfx;
extern crate fnv;
extern crate gfx_window_glutin;
extern crate glutin;
extern crate itertools;
extern crate nalgebra;
extern crate palette;
extern crate rand;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;

use fnv::FnvHashMap as HashMap;
use gfx::traits::FactoryExt;
use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::dpi::LogicalSize;
use glutin::Api::OpenGl;
use glutin::{GlContext, GlRequest};
use nalgebra::Vector2;
use palette::{Srgb, Srgba};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;

mod render;
mod state;

pub use glutin::VirtualKeyCode as Key;
pub use render::DrawState;
pub use state::{ExtendedState, Fill, PollockState, PollockStateOwned, Stroke};
pub use std::f64::consts;

type ColorFormat = gfx::format::Srgba8;
type DepthFormat = gfx::format::DepthStencil;

pub type V2 = Vector2<f64>;
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

// TODO: Use type parameters instead of Box<Fn>
/// Entry point for `Pollock`. This is a builder that allows you to write the
#[must_use = "Pollock does nothing until you call `.run()`"]
pub struct Pollock<'a, State, SetupFn, DrawFn, UpdateFn> {
    setup_fn: Option<SetupFn>,
    draw_fn: DrawFn,
    update_fn: UpdateFn,
    hot_reload: bool,
    key_handlers: HashMap<Key, KeyHandler<'a, State>>,
    frame_handlers: Vec<(usize, Box<FnMut(&mut PollockState<State>) + 'a>)>,
}

impl<'a, S, SFn>
    Pollock<
        'a,
        S,
        SFn,
        fn(&mut ExtendedState<&mut PollockState<&S>, DrawState>),
        fn(&mut PollockState<S>),
    >
where
    SFn: FnOnce(&mut PollockStateOwned<()>) -> S,
{
    pub fn setup(fun: SFn) -> Self {
        Pollock {
            setup_fn: Some(fun),
            key_handlers: Default::default(),
            frame_handlers: Default::default(),
            hot_reload: false,
            draw_fn: |_| {},
            update_fn: |_| {},
        }
    }
}

impl<'a, S, SFn, DFn, UFn> Pollock<'a, S, SFn, DFn, UFn>
where
    SFn: FnOnce(&mut PollockStateOwned<()>) -> S,
    DFn: FnMut(&mut ExtendedState<&mut PollockState<&S>, DrawState>),
    UFn: FnMut(&mut PollockState<S>),
    // TODO: Remove this bound if we're not hot reloading
    S: Serialize,
    for<'any> S: Deserialize<'any>,
{
    pub fn draw<F: FnMut(&mut ExtendedState<&mut PollockState<&S>, DrawState>) + 'a>(
        self,
        fun: F,
    ) -> Pollock<'a, S, SFn, F, UFn> {
        Pollock {
            setup_fn: self.setup_fn,
            key_handlers: self.key_handlers,
            frame_handlers: self.frame_handlers,
            update_fn: self.update_fn,
            hot_reload: self.hot_reload,
            draw_fn: fun,
        }
    }

    pub fn hot_reload(self) -> Pollock<'a, S, SFn, DFn, UFn> {
        Pollock {
            setup_fn: self.setup_fn,
            key_handlers: self.key_handlers,
            frame_handlers: self.frame_handlers,
            update_fn: self.update_fn,
            draw_fn: self.draw_fn,
            hot_reload: true,
        }
    }

    pub fn update<F: FnMut(&mut PollockState<S>) + 'a>(
        self,
        fun: F,
    ) -> Pollock<'a, S, SFn, DFn, F> {
        Pollock {
            setup_fn: self.setup_fn,
            key_handlers: self.key_handlers,
            frame_handlers: self.frame_handlers,
            draw_fn: self.draw_fn,
            hot_reload: self.hot_reload,
            update_fn: fun,
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

    // Seperate everything out so we get destructors run on hot reload
    fn run_internal(mut self) -> Option<std::path::PathBuf> {
        use gfx::Factory;
        use render::{pipe, Transform};
        use std::{env, fs, mem, time};

        let current_exe = env::current_exe().unwrap();
        let mut mod_time = if self.hot_reload {
            Some((
                fs::metadata(&current_exe).unwrap().modified().unwrap(),
                time::Instant::now(),
            ))
        } else {
            None
        };

        let mut state_owned: PollockStateOwned<S> = if let Ok(statefile) = env::var("POLLOCK_STATE")
        {
            // TODO: Handle this more gracefully
            let file =
                fs::File::open(&statefile).expect("Hot reload failed - state file non-existant");
            serde_json::from_reader(&file).expect("Hot reload failed - state file out of date")
        } else {
            let mut init_state = PollockStateOwned::new((), state::InternalState::default());

            let inner_state = (self.setup_fn.take().unwrap())(&mut init_state);

            init_state.with_state(inner_state)
        };

        let mut state: PollockState<S> =
            PollockState::new(state_owned.state, &mut state_owned.internal);

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

            let mut screen_tex = None;

            let mut run_context = true;
            let (mut cached_vertices, mut cached_indices) = (vec![], vec![]);
            let mut has_run_once = false;

            while run_application && run_context {
                if let Some((mod_time, last_check)) = &mut mod_time {
                    let now = time::Instant::now();

                    if now - *last_check > time::Duration::from_secs(1) {
                        if let Ok(modified) = fs::metadata(&current_exe).and_then(|m| m.modified())
                        {
                            println!("{:?}", modified);
                            if modified > *mod_time {
                                let filename = std::path::PathBuf::from("HOT_RELOAD_FILE");

                                let outfile = fs::File::create(&filename).unwrap();

                                serde_json::to_writer(&outfile, &state).unwrap();

                                return Some(filename);
                            } else {
                                *last_check = now;
                            }
                        }
                    }
                }

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
                                            if let Some(handler) = self
                                                .key_handlers
                                                .get_mut(&key)
                                                .and_then(|handler| handler.down.as_mut())
                                            {
                                                handler(&mut state);
                                            }
                                        }
                                        ElementState::Released => {
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
                            glutin::WindowEvent::Refresh => run_context = !has_run_once,
                            _ => {}
                        }
                    }
                });

                if has_run_once && state.paused {
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

                //Identity Matrix
                let scale = (2. / w).min(2. / h);
                let transform = Transform {
                    transform: [
                        [scale, 0.0, 0.0, -1.0],
                        [0.0, scale, 0.0, -1.0],
                        [0.0, 0.0, 1.0, 0.0],
                        [0.0, 0.0, 0.0, 1.0],
                    ],
                };

                // We cache the allocation for the vertices since we assume that it's rare that we will
                // reduce the number of vertices in a scene significantly over the course of a run.
                // If this assumption is violated we can shrink using heuristics like the average vec
                // size over the past N frames or whatever.
                //
                // See below for where we switch them back.
                let draw_state = RefCell::new(DrawState::new(
                    mem::replace(&mut cached_vertices, vec![]),
                    mem::replace(&mut cached_indices, vec![]),
                ));

                if !state.paused {
                    (self.update_fn)(&mut state);
                }

                let mut maybe_internal = if state.paused {
                    Some((*state).clone())
                } else {
                    None
                };

                let mut borrowed = if let Some(internal) = &mut maybe_internal {
                    PollockState::new(&state.state, internal)
                } else {
                    PollockState::new(&state.state, &mut state.internal)
                };
                let mut ex_state = borrowed.extend(&draw_state);
                (self.draw_fn)(&mut ex_state);

                let (vertex_buffer, slice) = {
                    let inner = ex_state.inner.borrow();

                    factory.create_vertex_buffer_with_slice(&inner.vertices[..], &inner.indices[..])
                };

                {
                    let inner = &mut *ex_state.inner.borrow_mut();
                    cached_vertices = mem::replace(&mut inner.vertices, vec![]);
                    cached_indices = mem::replace(&mut inner.indices, vec![]);
                    cached_vertices.clear();
                    cached_indices.clear();
                }

                let transform_buffer = factory.create_constant_buffer(1);

                let to_tex_data = state.save_frame.take().map(|path| {
                    use gfx::memory::{Bind, Usage};
                    use gfx::texture::{AaMode, Kind};

                    let tex = screen_tex.take().unwrap_or_else(|| {
                        factory
                            .create_texture::<gfx::format::R8_G8_B8_A8>(
                                Kind::D2(0, 0, AaMode::Single),
                                0,
                                Bind::RENDER_TARGET,
                                Usage::Download,
                                None,
                            ).unwrap()
                    });

                    (tex, path)
                });

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

                if let Some((tex, ..)) = &to_tex_data {
                    encoder.draw(
                        &slice,
                        &pso,
                        &pipe::Data {
                            vbuf: vertex_buffer.clone(),
                            transform: transform_buffer.clone(),
                            out: factory
                                .view_texture_as_render_target(&tex, 0, None)
                                .unwrap(),
                        },
                    );
                }

                encoder.flush(&mut device);

                window.swap_buffers().unwrap();
                device.cleanup();

                if let Some((tex, path)) = to_tex_data {
                    unimplemented!();
                }

                state.frame_count += 1;

                has_run_once = true;
            }
        }

        None
    }

    pub fn run(self) {
        use std::env;
        use std::os::unix::process::CommandExt;
        use std::process::Command;

        let current_exe = env::current_exe().unwrap();

        if let Some(written_path) = self.run_internal() {
            Command::new(current_exe)
                .args(env::args())
                .envs(env::vars().chain(::std::iter::once((
                    "POLLOCK_STATE".to_string(),
                    written_path.to_string_lossy().into_owned(),
                )))).exec();
        }
    }
}
