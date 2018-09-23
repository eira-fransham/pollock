#![feature(nll)]
extern crate glutin_window;
extern crate graphics;
extern crate nalgebra;
extern crate opengl_graphics;
extern crate palette;
extern crate piston;

use glutin_window::GlutinWindow as Window;
use graphics::{line, rectangle, Context, Graphics, Transformed};
use nalgebra::Vector2;
use opengl_graphics::{GlGraphics, OpenGL};
use palette::{Srgb, Srgba};
use piston::event_loop::*;
use piston::input::*;
use piston::window::WindowSettings;

type V2 = Vector2<f64>;
type Color = Srgba<u8>;

struct DrawState<'a> {
    context: Context,
    gl: &'a mut GlGraphics,
}

struct Stroke {
    pub color: Color,
    pub thickness: f64,
}

impl Default for Stroke {
    fn default() -> Self {
        Stroke {
            color: Color::new(0, 0, 0, 255),
            thickness: 1.,
        }
    }
}

impl Stroke {
    fn new<C: Into<Color>, T: Into<f64>>(color: C, thickness: T) -> Self {
        Stroke {
            color: color.into(),
            thickness: thickness.into(),
        }
    }

    fn none() -> Self {
        Stroke::new(Color::new(0, 0, 0, 0), 0)
    }
}

struct Fill {
    pub color: Color,
}

impl Default for Fill {
    fn default() -> Self {
        Fill {
            color: Color::new(255, 255, 255, 255),
        }
    }
}

impl Fill {
    fn flat<C: Into<Color>>(color: C) -> Self {
        Fill {
            color: color.into(),
        }
    }

    fn none() -> Self {
        Fill {
            color: Color::new(255, 255, 255, 0),
        }
    }
}

struct PollockState<State, Inner = ()> {
    pub state: State,
    pub stroke: Stroke,
    pub fill: Fill,
    pub background: Color,
    pub frame_count: usize,
    size: (u32, u32),
    size_dirty: bool,
    inner: Inner,
}

impl<S, I> PollockState<S, I> {
    fn with_inner<Inner>(self, inner: Inner) -> PollockState<S, Inner> {
        PollockState {
            state: self.state,
            stroke: self.stroke,
            fill: self.fill,
            size: self.size,
            size_dirty: self.size_dirty,
            background: self.background,
            frame_count: self.frame_count,
            inner,
        }
    }

    fn size(&mut self, w: u32, h: u32) {
        self.size = (w, h);
        self.size_dirty = true;
    }

    fn width(&self) -> u32 {
        self.size.0
    }

    fn height(&self) -> u32 {
        self.size.0
    }
}

impl<S> PollockState<S, ()> {
    fn new(state: S) -> Self {
        PollockState {
            state,
            stroke: Default::default(),
            fill: Default::default(),
            background: Color::new(0, 0, 0, 0),
            frame_count: 0,
            size: (640, 480),
            size_dirty: true,
            inner: (),
        }
    }
}

impl<'a, S> PollockState<S, DrawState<'a>> {
    fn rect<W: Into<f64>, H: Into<f64>>(&mut self, pos: V2, width: W, height: H) {
        let shape = rectangle::Rectangle {
            color: [
                self.fill.color.color.red as f32 / 255.0,
                self.fill.color.color.green as f32 / 255.0,
                self.fill.color.color.blue as f32 / 255.0,
                self.fill.color.alpha as f32 / 255.0,
            ],
            shape: rectangle::Shape::Square,
            border: if self.stroke.thickness != 0. {
                Some(rectangle::Border {
                    color: [
                        self.stroke.color.color.red as f32 / 255.0,
                        self.stroke.color.color.green as f32 / 255.0,
                        self.stroke.color.color.blue as f32 / 255.0,
                        self.stroke.color.alpha as f32 / 255.0,
                    ],
                    radius: self.stroke.thickness,
                })
            } else {
                None
            },
        };

        self.inner.gl.rectangle(
            &shape,
            [pos.x, pos.y, width.into(), height.into()],
            &Default::default(),
            self.inner.context.transform,
        );
    }

    fn line(&mut self, a: V2, b: V2) {
        self.inner.gl.line(
            &line::Line {
                color: [
                    self.stroke.color.color.red as f32 / 255.0,
                    self.stroke.color.color.green as f32 / 255.0,
                    self.stroke.color.color.blue as f32 / 255.0,
                    self.stroke.color.alpha as f32 / 255.0,
                ],
                radius: self.stroke.thickness,
                shape: line::Shape::Square,
            },
            [a.x, a.y, b.x, b.y],
            &Default::default(),
            self.inner.context.transform,
        );
    }

    fn lines<I: IntoIterator<Item = V2>>(&mut self, lines: I) {
        let mut lines = lines.into_iter();
        if let Some(mut last) = lines.next() {
            for cur in lines {
                self.line(last, cur);
                last = cur;
            }
        }
    }
}

// TODO: Use type parameters instead of Box<Fn>
#[must_use = "The Pollock struct does nothing until you call `.run()`"]
struct Pollock<'a, State> {
    state: State,
    setup_fn: Option<Box<FnMut(&mut PollockState<State, ()>) + 'a>>,
    draw_fn: Option<Box<FnMut(&mut PollockState<State, DrawState>) + 'a>>, // TODO: Use type parameter
}

impl<'a> Pollock<'a, ()> {
    fn new() -> Self {
        Self::with_state(())
    }
}

impl<'a, S> Pollock<'a, S> {
    fn with_state(state: S) -> Self {
        Pollock {
            state,
            setup_fn: None,
            draw_fn: None,
        }
    }

    fn draw<F: FnMut(&mut PollockState<S, DrawState>) + 'a>(self, fun: F) -> Self {
        Pollock {
            state: self.state,
            setup_fn: self.setup_fn,
            draw_fn: Some(Box::new(fun) as _),
        }
    }

    fn setup<F: FnMut(&mut PollockState<S, ()>) + 'a>(self, fun: F) -> Self {
        Pollock {
            state: self.state,
            setup_fn: Some(Box::new(fun) as _),
            draw_fn: self.draw_fn,
        }
    }

    fn run(mut self) {
        // Change this to OpenGL::V2_1 if not working.
        let opengl = OpenGL::V3_2;

        let mut state = PollockState::new(self.state);

        if let Some(mut setup_fn) = self.setup_fn {
            setup_fn(&mut state);
        }

        // Create an Glutin window.
        let mut window: Window = WindowSettings::new("Pollock", [state.size.0, state.size.1])
            .opengl(opengl)
            .exit_on_esc(true)
            .build()
            .unwrap();

        let mut state = Some(state);
        let mut gl = GlGraphics::new(opengl);

        let mut events = Events::new(EventSettings::new());
        while let Some(e) = events.next(&mut window) {
            if let Some(state) = state.as_mut() {
                if state.size_dirty {
                    let size = Some(state.size);
                    window.window.set_min_dimensions(size);
                    window.window.set_max_dimensions(size);
                    window.window.set_inner_size(state.size.0, state.size.1);

                    state.size_dirty = false;
                }
            }

            if let Some(r) = e.render_args() {
                let context = gl.draw_begin(r.viewport());

                let new_state = state.take().map(|state| {
                    state.with_inner(DrawState {
                        gl: &mut gl,
                        context,
                    })
                });

                let new_state = if let (Some(draw_fn), Some(mut new_state)) =
                    (self.draw_fn.as_mut(), new_state)
                {
                    new_state.inner.gl.clear_color([
                        new_state.background.color.red as f32 / 255.,
                        new_state.background.color.green as f32 / 255.,
                        new_state.background.color.blue as f32 / 255.,
                        new_state.background.alpha as f32 / 255.,
                    ]);
                    draw_fn(&mut new_state);
                    new_state.frame_count += 1;
                    Some(new_state)
                } else {
                    None
                };

                state = new_state.map(|state| state.with_inner(()));

                gl.draw_end();
            }

            if let Some(u) = e.update_args() {
                // app.update(&u);
            }
        }
    }
}

fn main() {
    Pollock::new()
        .setup(|p| {
            p.size(600, 600);
            p.background = Color::new(255, 255, 255, 255);
        }).draw(|p| {
            for j in 0..10u64 {
                for i in 0..10u64 {
                    p.stroke = Stroke::new(Srgb::new(0, 0, (25 * i) as u8), 1);
                    p.line(
                        V2::new(
                            (i * 60) as f64,
                            300. + 100. * (i as f64 + j as f64 + p.frame_count as f64 / 40.).sin(),
                        ),
                        V2::new(
                            ((i + 1) * 60) as f64,
                            300. + 100.
                                * (i as f64 + j as f64 + p.frame_count as f64 / 40. + 1.).sin(),
                        ),
                    )
                }
            }
        }).run();
}
