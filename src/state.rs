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
        }
    }
}

impl Default for InternalState {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait to abstract over a modified and a non-modified `PollockState`.
/// It's used to implement the transformation stack at compile-time instead
/// of runtime.
pub trait DrawParams {
    /// Get the currently-defined stroke
    fn stroke(&self) -> Stroke;
    /// Get the currently-defined fill
    fn fill(&self) -> Fill;
    /// Get the currently-defined transformation
    fn transform(&self) -> &Transform;
}

/// A "modified" state - one where changes to the transformation, the stroke
/// and the fill will be removed when this value goes out of scope.
pub struct StateWithModifications<'a, PState> {
    delegate_to: &'a PState,
    /// The transformation - for more information see the `transform` field
    /// of `PollockState`.
    pub transform: Transform,
    /// The stroke - for more information see the `stroke` field
    /// of `PollockState`.
    pub stroke: Stroke,
    /// The fill - for more information see the `fill` field
    /// of `PollockState`.
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

    /// Append a rotation to the current transformation. See `PollockState::rotate` for more information.
    #[inline]
    pub fn rotate(&mut self, radians: f64) {
        self.transform = Transform::new_rotation(radians) * self.transform;
    }

    /// Append a translation to the current transformation. See `PollockState::translate` for more information.
    #[inline]
    pub fn translate(&mut self, translate: V2) {
        self.transform = Transform::new_translation(&translate) * self.transform;
    }

    /// Append a scale to the current transformation. See `PollockState::scale` for more information.
    #[inline]
    pub fn scale<Scl: Scale>(&mut self, scale: Scl) {
        self.transform = Transform::new_nonuniform_scaling(&scale.into_scale()) * self.transform;
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

/// This trait represents a type that can be interpreted as a scale vector.
/// Essentially, for a 2D vector it just returns itself, for a scalar it
/// returns a vector with both elements set to the same thing.
pub trait Scale {
    /// Convert this type into a scale vector.
    fn into_scale(self) -> V2;
}

impl Scale for V2 {
    fn into_scale(self) -> V2 {
        self
    }
}

macro_rules! impl_scale {
    ($($t:ty),*) => {
        $(
            impl Scale for $t {
                fn into_scale(self) -> V2 {
                    v2(self, self)
                }
            }
        )*
    };
}

impl_scale!(f32, f64, u8, u16, u32);

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

    /// Append a rotation to the current transformation. Note that because the transformations work
    /// as a stack this might not do what you expect. Doing `p.translate(...)` and then calling
    /// `p.rotate(...)` will rotate the translation, too! If you don't want to rotate the translation
    /// too, you can call `p.rotate(...)` first, and `p.translate(...)` afterwards. Also note that
    /// unlike setting, for example, the fill color, calling this multiple times will append multiple
    /// rotations. Calling `p.rotate(1)` 3 times is the same as calling `p.rotate(3)`. To temporarily
    /// rotate, you can use the `push` function in `ExtendedState`.
    #[inline]
    pub fn rotate(&mut self, radians: f64) {
        self.transform = Transform::new_rotation(radians) * self.transform;
    }

    /// Append a translation to the current transformation. A translation is just a movement
    /// in 2D space. After calling this, all objects drawn will be offset by the given vector.
    /// Unlike setting, for example, the fill color, calling this multiple times will append
    /// multiple translations. Calling `p.translate(v2(1, 0))` 3 times is the same as calling
    /// `p.translate(v2(3, 0))`. To temporarily translate, you can use the `push` function in
    /// `ExtendedState`.
    #[inline]
    pub fn translate(&mut self, translate: V2) {
        self.transform = Transform::new_translation(&translate) * self.transform;
    }

    /// Append a scale to the current transformation. Note that because the transformations work
    /// as a stack this might not do what you expect. Doing `p.translate(...)` and then calling
    /// `p.scale(...)` will scale up the translation, too! If you don't want to scale the translation
    /// too, you can call `p.scale(...)` first, and `p.translate(...)` afterwards. Also note that
    /// unlike setting, for example, the fill color, calling this multiple times will scale multiple
    /// times. To temporarily scale, you can use the `push` function in `ExtendedState`.
    #[inline]
    pub fn scale<Scl: Scale>(&mut self, scale: Scl) {
        self.transform = Transform::new_nonuniform_scaling(&scale.into_scale()) * self.transform;
    }

    /// Take a screenshot and save it to the given path (relative to the directory that the process
    /// was started from). Because rendering is batched in Pollock, this will take the screenshot
    /// at the end of the frame, and so calling it in the middle of drawing will not take a screenshot
    /// of a partially-rendered screen. If you want to take a screenshot of a partially-rendered screen,
    /// you will have to do it over the course of multiple frames.
    #[inline]
    pub fn save_image<P: AsRef<Path>>(&mut self, filename: P) {
        self.save_frame = Some(filename.as_ref().into());
    }

    /// Start recording a movie. This will open a folder in your operating system's temp directory
    /// and take a screenshot each frame until you call `stop_recording`.
    #[inline]
    pub fn start_recording(&mut self) {
        use tempfile::tempdir;

        self.record_folder = Some(tempdir().unwrap().into_path());
    }

    /// Stop recording and save the resulting movie to a gif at the given path. After the gif has been
    /// saved, it will delete all the frames.
    ///
    /// This function does nothing if we are not recording.
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

    /// The width of the screen, in pixels.
    #[inline]
    pub fn width(&self) -> u32 {
        self.size.0
    }

    /// The height of the screen, in pixels.
    #[inline]
    pub fn height(&self) -> u32 {
        self.size.1
    }

    /// A random value of the supplied type between `low` and `high`. This works for many
    /// different types. Normally Rust will figure it out from the context and the types passed
    /// as `low` and `high`, but if it says that it's unable to infer the type you can use the
    /// wonderfully-named "turbofish" syntax: `p.random_range::<f64>(0., 100.)`.
    #[inline]
    pub fn random_range<T: PartialOrd + SampleUniform>(&mut self, low: T, high: T) -> T {
        self.random.gen_range(low, high)
    }

    /// A random value of the supplied type between `low` and `high`. This works for many
    /// different types. Normally Rust will figure it out from the context, but if it says
    /// that it's unable to infer the type you can use the wonderfully-named "turbofish"
    /// syntax: `p.random::<u64>()`.
    #[inline]
    pub fn random<T>(&mut self) -> T
    where
        Standard: Distribution<T>,
    {
        self.random.gen()
    }
}

impl<S: Serialize> PollockState<S> {
    /// Save the state of the program (frame number, fill, stroke, random number state, etc).
    /// If you are using the `state` field of `PollockState` and the state can be serialized, this
    /// will be saved too. If the state can't be saved, Rust will throw an error at compile-time. If
    /// you are persisting state between frames using global variables, these will not be saved.
    ///
    /// The path is relative to the directory that this program was executed from.
    pub fn save_state<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let file = fs::File::create(path)?;

        Ok(serde_json::to_writer(&file, self)?)
    }
}

impl<S: for<'any> Deserialize<'any>> PollockState<S> {
    /// Load the state of the program from a file (frame number, fill, stroke, random number
    /// state, etc). If you are using the `state` variable and the state can be deserialized,
    /// this will be loaded too. If the state can't be loaded, Rust will throw an error at
    /// compile-time. If you are persisting state between frames using global variables, these will
    /// not be loaded.
    ///
    /// The path is relative to the directory that this program was executed from.
    pub fn load_state<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let file = fs::File::open(path)?;

        let new: PollockState<S> = serde_json::from_reader(&file)?;
        *self = new;

        Ok(())
    }
}

/// The `PollockState` "extended" with extra functionality. Currently the only available extra
/// functionality is drawing, which is used in the argument to the `draw` closure. The `PState`
/// variable is the kind of PollockState that we're using. This is an internal detail and you
/// normally will not have to worry about this type at all, let alone its parameters.
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
