use rand::distributions::uniform::SampleUniform;
use rand::distributions::{Distribution, Standard};
use rand::prng::XorShiftRng;
use rand::{FromEntropy, Rng};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::{fs, io};
use {serde_json, Color};

#[derive(Copy, Clone, Serialize, Deserialize)]
pub struct Stroke {
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
    pub fn new<C: Into<Color>, T: Into<f64>>(color: C, thickness: T) -> Self {
        Stroke {
            color: color.into(),
            thickness: thickness.into(),
        }
    }

    pub fn none() -> Self {
        Stroke::new(Color::new(0, 0, 0, 0), 0)
    }

    pub fn is_none(&self) -> bool {
        self.thickness == 0.
    }
}

#[derive(Copy, Clone, Serialize, Deserialize)]
pub struct Fill {
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
    pub fn new<C: Into<Color>>(color: C) -> Self {
        Fill {
            color: color.into(),
        }
    }

    pub fn none() -> Self {
        Fill {
            color: Color::new(255, 255, 255, 0),
        }
    }

    pub fn is_none(&self) -> bool {
        self.color.alpha == 0
    }
}

#[derive(Serialize, Deserialize)]
pub struct PollockStateGeneric<Internal, State> {
    pub state: State,
    pub(crate) internal: Internal,
}

pub type PollockState<'a, State> = PollockStateGeneric<&'a mut InternalState, State>;
pub type PollockStateOwned<State> = PollockStateGeneric<InternalState, State>;

impl<'a, State> Deref for PollockState<'a, State> {
    type Target = InternalState;

    fn deref(&self) -> &Self::Target {
        &*self.internal
    }
}

impl<'a, State> DerefMut for PollockState<'a, State> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.internal
    }
}

impl<State> Deref for PollockStateOwned<State> {
    type Target = InternalState;

    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl<State> DerefMut for PollockStateOwned<State> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.internal
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct InternalState {
    pub paused: bool,
    pub stroke: Stroke,
    pub fill: Fill,
    pub background: Color,
    pub frame_count: usize,
    pub size: (u32, u32),
    pub(crate) save_frame: Option<PathBuf>,
    random: XorShiftRng,
    cached_size: (u32, u32),
}

impl InternalState {
    fn new() -> Self {
        let size = (640, 480);
        InternalState {
            save_frame: None,
            paused: false,
            stroke: Default::default(),
            fill: Default::default(),
            random: <_>::from_entropy(),
            background: Color::new(0, 0, 0, 0),
            frame_count: 0,
            size: size,
            cached_size: size,
        }
    }
}

impl Default for InternalState {
    fn default() -> Self {
        Self::new()
    }
}

pub trait DrawParams {
    fn stroke(&self) -> Stroke;
    fn fill(&self) -> Fill;
}

pub struct StateWithModifications<'a, S> {
    delegate_to: &'a PollockState<'a, S>,
    stroke: Option<Stroke>,
    fill: Option<Fill>,
}

impl<'a, S> Clone for StateWithModifications<'a, S> {
    fn clone(&self) -> Self {
        StateWithModifications {
            delegate_to: self.delegate_to,
            stroke: self.stroke,
            fill: self.fill,
        }
    }
}

impl<'a, S> StateWithModifications<'a, S> {
    pub(crate) fn new(state: &'a PollockState<S>) -> Self {
        StateWithModifications {
            delegate_to: state,
            stroke: None,
            fill: None,
        }
    }
}

impl<'a, PState> StateWithModifications<'a, PState> {
    pub fn with_stroke(mut self, stroke: Stroke) -> Self {
        self.stroke = Some(stroke);
        self
    }
    pub fn with_fill(mut self, fill: Fill) -> Self {
        self.fill = Some(fill);
        self
    }
}

impl<'a, S> DrawParams for StateWithModifications<'a, S> {
    fn stroke(&self) -> Stroke {
        self.stroke.unwrap_or(self.delegate_to.stroke)
    }
    fn fill(&self) -> Fill {
        self.fill.unwrap_or(self.delegate_to.fill)
    }
}

impl<'a, S> DrawParams for PollockState<'a, S> {
    fn stroke(&self) -> Stroke {
        self.stroke
    }
    fn fill(&self) -> Fill {
        self.fill
    }
}

impl<'a, T> DrawParams for &'a T
where
    T: DrawParams,
{
    fn stroke(&self) -> Stroke {
        (**self).stroke()
    }
    fn fill(&self) -> Fill {
        (**self).fill()
    }
}

impl<'a, T> DrawParams for &'a mut T
where
    T: DrawParams,
{
    fn stroke(&self) -> Stroke {
        (**self).stroke()
    }
    fn fill(&self) -> Fill {
        (**self).fill()
    }
}

impl<Internal, S> PollockStateGeneric<Internal, S> {
    pub(crate) fn extend<'a, 'b, I>(
        &'b mut self,
        inner: &'a RefCell<I>,
    ) -> ExtendedState<'a, &'b mut Self, I> {
        ExtendedState { state: self, inner }
    }

    #[inline]
    pub(crate) fn with_state<State>(self, state: State) -> PollockStateGeneric<Internal, State> {
        PollockStateGeneric {
            state,
            internal: self.internal,
        }
    }

    #[inline]
    pub(crate) fn new(state: S, internal: Internal) -> Self {
        PollockStateGeneric {
            state,
            internal: internal,
        }
    }
}

// TODO: `push_transform`/`pop_transform`
impl<I, S> PollockStateGeneric<I, S>
where
    Self: DerefMut<Target = InternalState>,
{
    pub(crate) fn size_dirty(&self) -> bool {
        self.cached_size != self.size
    }

    pub(crate) fn refresh_size(&mut self) {
        self.cached_size = self.size;
    }

    #[inline]
    pub fn save_image<P: AsRef<Path>>(&mut self, filename: P) {
        self.save_frame = Some(filename.as_ref().into());
    }

    #[inline]
    pub fn width(&self) -> u32 {
        self.size.0
    }

    #[inline]
    pub fn height(&self) -> u32 {
        self.size.0
    }

    #[inline]
    pub fn random_range<T: PartialOrd + SampleUniform>(&mut self, low: T, high: T) -> T {
        self.random.gen_range(low, high)
    }

    #[inline]
    pub fn random<T>(&mut self) -> T
    where
        Standard: Distribution<T>,
    {
        self.random.gen()
    }
}

impl<'a, S: Serialize> PollockState<'a, S> {
    pub fn save_state<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let file = fs::File::create(path)?;

        Ok(serde_json::to_writer(&file, self)?)
    }
}

impl<'a, S: for<'any> Deserialize<'any>> PollockState<'a, S> {
    pub fn load_state<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let file = fs::File::open(path)?;

        let new: PollockStateOwned<S> = serde_json::from_reader(&file)?;
        self.state = new.state;
        *self.internal = new.internal;

        Ok(())
    }
}

pub struct ExtendedState<'a, PState, I> {
    pub(crate) state: PState,
    pub(crate) inner: &'a RefCell<I>,
}

impl<'a, PState, I> Deref for ExtendedState<'a, PState, I> {
    type Target = PState;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'a, PState, I> DerefMut for ExtendedState<'a, PState, I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}
