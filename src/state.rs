pub struct DrawState<'a> {
    context: Context,
    gl: &'a mut GlGraphics,
}

#[derive(Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
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
    pub fn flat<C: Into<Color>>(color: C) -> Self {
        Fill {
            color: color.into(),
        }
    }

    pub fn none() -> Self {
        Fill {
            color: Color::new(255, 255, 255, 0),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct PollockState<State> {
    pub state: State,
    pub stroke: Stroke,
    pub fill: Fill,
    pub offset: V2,
    pub rotation: f64,
    pub background: Color,
    pub frame_count: usize,
    random: XorShiftRng,
    size: (u32, u32),
    size_dirty: bool,
}

impl<S> PollockState<S> {
    pub fn size(&mut self, w: u32, h: u32) {
        self.size = (w, h);
        self.size_dirty = true;
    }

    pub fn width(&self) -> u32 {
        self.size.0
    }

    pub fn height(&self) -> u32 {
        self.size.0
    }

    pub fn random_range<T: PartialOrd + SampleUniform>(&mut self, low: T, high: T) -> T {
        self.random.gen_range(low, high)
    }

    pub fn random<T>(&mut self) -> T
    where
        Standard: Distribution<T>,
    {
        self.random.gen()
    }

    fn extend<I>(&mut self, inner: I) -> ExtendedState<S, I> {
        ExtendedState {
            state: self,
            inner: RefCell::new(inner),
        }
    }

    fn with_state<State>(self, state: State) -> PollockState<State> {
        PollockState {
            state,
            stroke: self.stroke,
            fill: self.fill,
            offset: self.offset,
            rotation: self.rotation,
            random: self.random,
            background: self.background,
            frame_count: self.frame_count,
            size: self.size,
            size_dirty: self.size_dirty,
        }
    }

    fn new(state: S) -> Self {
        PollockState {
            state,
            stroke: Default::default(),
            fill: Default::default(),
            offset: v2(0, 0),
            rotation: 0.,
            random: <_>::from_entropy(),
            background: Color::new(0, 0, 0, 0),
            frame_count: 0,
            size: (640, 480),
            size_dirty: true,
        }
    }
}

impl<S: Serialize> PollockState<S> {
    pub fn save_state<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let file = fs::File::create(path)?;

        Ok(serde_json::to_writer(&file, self)?)
    }
}

impl<S: for<'a> Deserialize<'a>> PollockState<S> {
    pub fn load_state<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let file = fs::File::open(path)?;

        let new = serde_json::from_reader(&file)?;

        *self = new;

        Ok(())
    }
}

pub struct ExtendedState<'a, S, I> {
    state: &'a mut PollockState<S>,
    inner: RefCell<I>,
}

impl<'a, S, I> Deref for ExtendedState<'a, S, I> {
    type Target = PollockState<S>;

    fn deref(&self) -> &Self::Target {
        &*self.state
    }
}

impl<'a, S, I> DerefMut for ExtendedState<'a, S, I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.state
    }
}

