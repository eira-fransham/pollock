use fnv::FnvHashSet as HashSet;
use rand::distributions::uniform::SampleUniform;
use rand::distributions::{Distribution, Standard};
use rand::prng::XorShiftRng;
use rand::{FromEntropy, Rng};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::{fs, io};
use {serde_json, v2, Color, Key, Transform, V2};

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
pub struct PollockState<State> {
    pub state: State,
    pub(crate) internal: InternalState,
}

impl<S> Deref for PollockState<S> {
    type Target = InternalState;

    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl<S> DerefMut for PollockState<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.internal
    }
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct KeySet {
    // We use `u16` instead of `Key` since `Key` doesn't implement
    // `Serialize + Deserialize`
    keys: HashSet<u16>,
}

impl KeySet {
    pub fn is_down(&self, key: Key) -> bool {
        self.keys.contains(&(key as u16))
    }

    pub(crate) fn press(&mut self, key: Key) {
        self.keys.insert(key as u16);
    }

    pub(crate) fn release(&mut self, key: Key) {
        self.keys.remove(&(key as u16));
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
    pub keys: KeySet,
    pub transform: Transform,
    pub(crate) save_frame: Option<PathBuf>,
    pub(crate) record_folder: Option<PathBuf>,
    random: XorShiftRng,
    cached_size: (u32, u32),
}

impl InternalState {
    fn new() -> Self {
        let size = (640, 480);
        InternalState {
            save_frame: None,
            record_folder: None,
            paused: false,
            stroke: Default::default(),
            fill: Default::default(),
            random: <_>::from_entropy(),
            background: Color::new(0, 0, 0, 0),
            frame_count: 0,
            keys: Default::default(),
            transform: Transform::identity(),
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
    fn transform(&self) -> &Transform;
}

pub struct StateWithModifications<'a, PState> {
    delegate_to: &'a PState,
    pub transform: Transform,
    pub stroke: Stroke,
    pub fill: Fill,
}

impl<'a, PState> Clone for StateWithModifications<'a, PState> {
    fn clone(&self) -> Self {
        StateWithModifications {
            delegate_to: self.delegate_to,
            transform: self.transform,
            stroke: self.stroke,
            fill: self.fill,
        }
    }
}

impl<'a, S> Deref for StateWithModifications<'a, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        self.delegate_to
    }
}

impl<'a, PState> StateWithModifications<'a, PState>
where
    PState: DrawParams,
{
    pub(crate) fn new(state: &'a PState) -> Self {
        let (transform, stroke, fill) = (state.transform().clone(), state.stroke(), state.fill());
        StateWithModifications {
            delegate_to: state,
            transform,
            stroke,
            fill,
        }
    }

    #[inline]
    pub fn rotate(&mut self, radians: f64) {
        self.transform *= Transform::new_rotation(radians);
    }

    #[inline]
    pub fn translate(&mut self, translate: V2) {
        self.transform *= Transform::new_translation(&translate);
    }

    #[inline]
    pub fn scale<Scl: Scale>(&mut self, scale: Scl) {
        self.transform *= Transform::new_nonuniform_scaling(&scale.into_scale());
    }
}

impl<'a, S> DrawParams for StateWithModifications<'a, S> {
    fn stroke(&self) -> Stroke {
        self.stroke
    }
    fn fill(&self) -> Fill {
        self.fill
    }
    fn transform(&self) -> &Transform {
        &self.transform
    }
}

impl<S> DrawParams for PollockState<S> {
    fn stroke(&self) -> Stroke {
        self.stroke
    }
    fn fill(&self) -> Fill {
        self.fill
    }
    fn transform(&self) -> &Transform {
        &self.transform
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
    fn transform(&self) -> &Transform {
        (**self).transform()
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
    fn transform(&self) -> &Transform {
        (**self).transform()
    }
}

pub trait Scale {
    fn into_scale(self) -> V2;
}

impl Scale for V2 {
    fn into_scale(self) -> V2 {
        self
    }
}

impl Scale for f64 {
    fn into_scale(self) -> V2 {
        v2(self, self)
    }
}

impl<S> PollockState<S> {
    pub(crate) fn extend<'a, 'b, I>(
        &'b mut self,
        inner: &'a RefCell<I>,
    ) -> ExtendedState<'a, &'b mut Self, I> {
        ExtendedState { state: self, inner }
    }

    #[inline]
    pub(crate) fn with_state<State>(self, state: State) -> PollockState<State> {
        PollockState {
            state,
            internal: self.internal,
        }
    }

    #[inline]
    pub(crate) fn new(state: S, internal: InternalState) -> Self {
        PollockState {
            state,
            internal: internal,
        }
    }

    pub(crate) fn size_dirty(&self) -> bool {
        self.cached_size != self.size
    }

    pub(crate) fn refresh_size(&mut self) {
        self.cached_size = self.size;
    }

    pub(crate) fn save_paths(&mut self) -> impl Iterator<Item = PathBuf> + '_ {
        let count = self.frame_count;
        self.save_frame
            .take()
            .into_iter()
            .chain(self.record_folder.iter().map(move |dir| {
                let mut dir = dir.to_owned();
                dir.push(count.to_string());
                dir
            }))
    }

    pub fn rotate(&mut self, radians: f64) {
        self.transform *= Transform::new_rotation(radians);
    }

    pub fn translate(&mut self, translate: V2) {
        self.transform *= Transform::new_translation(&translate);
    }

    pub fn scale<Scl: Scale>(&mut self, scale: Scl) {
        self.transform *= Transform::new_nonuniform_scaling(&scale.into_scale());
    }

    #[inline]
    pub fn save_image<P: AsRef<Path>>(&mut self, filename: P) {
        self.save_frame = Some(filename.as_ref().into());
    }

    #[inline]
    pub fn start_recording(&mut self) {
        use tempfile::tempdir;

        self.record_folder = Some(tempdir().unwrap().into_path());
    }

    /// This function does nothing if we are not recording
    #[inline]
    pub fn stop_recording<P: AsRef<Path>>(&mut self, filename: P) {
        use crossbeam::thread;
        use gifski;
        use std::fs;

        let filename = filename.as_ref().to_owned();
        if let Some(folder) = self.record_folder.take() {
            let out = fs::File::create(&filename).expect("Couldn't create gif output file");
            let iterator = fs::read_dir(&folder).expect("Couldn't find temp recording folder");

            let (mut collector, writer) = gifski::new(gifski::Settings {
                width: None,
                height: None,
                quality: 30,
                once: false,
                fast: false,
            }).unwrap();

            let thread_folder = folder.clone();
            thread::scope(move |scope| {
                let folder = thread_folder;
                let mut files = iterator.map(|f| f.unwrap().file_name()).collect::<Vec<_>>();
                files.sort();
                scope.spawn(move || {
                    for (i, file) in files.into_iter().enumerate() {
                        let name = folder.join(file);

                        collector.add_frame_png_file(i, name, 2).unwrap();
                    }
                });
                scope.spawn(move || {
                    println!("Saving gif...");
                    writer
                        .write(out, &mut gifski::progress::ProgressBar::new(100))
                        .unwrap();
                });
            });

            fs::remove_dir_all(folder).unwrap();
        }
    }

    #[inline]
    pub fn width(&self) -> u32 {
        self.size.0
    }

    #[inline]
    pub fn height(&self) -> u32 {
        self.size.1
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

impl<S: Serialize> PollockState<S> {
    pub fn save_state<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let file = fs::File::create(path)?;

        Ok(serde_json::to_writer(&file, self)?)
    }
}

impl<S: for<'any> Deserialize<'any>> PollockState<S> {
    pub fn load_state<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let file = fs::File::open(path)?;

        let new: PollockState<S> = serde_json::from_reader(&file)?;
        *self = new;

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
