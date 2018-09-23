#![feature(nll)]

#[macro_use]
extern crate gfx;
extern crate fnv;
extern crate gfx_window_glutin;
extern crate glutin;
extern crate nalgebra;
extern crate palette;
extern crate rand;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;

use gfx::traits::FactoryExt;
use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::dpi::LogicalSize;
use glutin::Api::OpenGl;
use glutin::{GlContext, GlRequest};

use fnv::FnvHashMap as HashMap;
use nalgebra::Vector2;
use palette::{Srgb, Srgba};
use rand::distributions::uniform::SampleUniform;
use rand::distributions::{Distribution, Standard};
use rand::prng::XorShiftRng;
use rand::{FromEntropy, Rng};
use serde::{Deserialize, Serialize};

pub use glutin::VirtualKeyCode as Key;

use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::{fs, io};

mod render;
mod state;

pub use render::DrawState;
pub use state::{ExtendedState, Fill, PollockState, Stroke};

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

struct KeyHandler<'a, State> {
    down: Option<Box<FnMut(&mut PollockState<State>) + 'a>>,
    up: Option<Box<FnMut(&mut PollockState<State>) + 'a>>,
}

// TODO: Use type parameters instead of Box<Fn>
#[must_use = "Pollock does nothing until you call `.run()`"]
pub struct Pollock<'a, State, SetupFn> {
    setup_fn: Option<SetupFn>,
    draw_fn: Box<FnMut(&mut ExtendedState<State, DrawState>) + 'a>,
    key_handlers: HashMap<Key, KeyHandler<'a, State>>,
}

impl<'a, S, SFn> Pollock<'a, S, SFn>
where
    SFn: FnOnce(&mut PollockState<()>) -> S,
{
    pub fn draw<F: FnMut(&mut ExtendedState<S, DrawState>) + 'a>(
        self,
        fun: F,
    ) -> Pollock<'a, S, SFn> {
        Pollock {
            setup_fn: self.setup_fn,
            key_handlers: self.key_handlers,
            draw_fn: Box::new(fun),
        }
    }

    pub fn setup(fun: SFn) -> Self {
        Pollock {
            setup_fn: Some(fun),
            key_handlers: Default::default(),
            draw_fn: Box::new(|_| {}),
        }
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
        let mut init_state = PollockState::new(());

        let inner_state = (self.setup_fn.take().unwrap())(&mut init_state);

        let mut state: PollockState<S> = init_state.with_state(inner_state);

        let mut events_loop = glutin::EventsLoop::new();
        let windowbuilder = glutin::WindowBuilder::new()
            .with_title("Pollock")
            .with_dimensions(LogicalSize::new(state.width(), state.height()));
        let contextbuilder = glutin::ContextBuilder::new()
            .with_gl(GlRequest::Specific(OpenGl, (3, 2)))
            .with_vsync(true);
        let (window, mut device, mut factory, color_view, mut depth_view) =
            gfx_glutin::init::<ColorFormat, DepthFormat>(
                windowbuilder,
                contextbuilder,
                &events_loop,
            );

        let mut running = true;
        while running {
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
                        } => running = false,
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
                        glutin::WindowEvent::Refresh => {
                            let mut ex_state = state.extend(DrawState::new());

                            (self.draw_fn)(&mut ex_state);
                            ex_state.frame_count += 1;
                        }
                        _ => {}
                    }
                }
            });

            if state.size_dirty {
                let size = LogicalSize::new(state.width(), state.height());
                window.set_min_dimensions(Some(size));
                window.set_max_dimensions(Some(size));
                window.set_inner_size(size);

                state.size_dirty = false;
            }
        }
    }
}
