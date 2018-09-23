use fnv::FnvHashMap as HashMap;
use gfx::traits::FactoryExt;
use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::Api::OpenGl;
use glutin::{GlContext, GlRequest};
use nalgebra::Vector2;
use palette::{Srgb, Srgba};
use rand::distributions::uniform::SampleUniform;
use rand::distributions::{Distribution, Standard};
use rand::prng::XorShiftRng;
use rand::{FromEntropy, Rng};
use serde::{Deserialize, Serialize};
use state::ExtendedState;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::{fs, io};
use {Color, V2};

#[derive(Default)]
pub struct DrawState<'a> {
    _marker: ::std::marker::PhantomData<&'a mut ()>,
}

impl<'a> DrawState<'a> {
    pub(crate) fn new() -> Self {
        Default::default()
    }
}

impl<'a, 'b, S> ExtendedState<'a, S, DrawState<'b>> {
    fn fill_color(&self) -> [u8; 4] {
        [
            self.fill.color.color.red,
            self.fill.color.color.green,
            self.fill.color.color.blue,
            self.fill.color.alpha,
        ]
    }

    fn stroke_color(&self) -> [u8; 4] {
        [
            self.stroke.color.color.red,
            self.stroke.color.color.green,
            self.stroke.color.color.blue,
            self.stroke.color.alpha,
        ]
    }

    pub fn circle<R: Into<f64>>(&self, pos: V2, rad: R) {
        let rad = rad.into();
        self.ellipse(pos, rad, rad)
    }

    pub fn ellipse<RX: Into<f64>, RY: Into<f64>>(&self, pos: V2, rad_x: RX, rad_y: RY) {
        unimplemented!()
    }

    pub fn rect<W: Into<f64>, H: Into<f64>>(&self, pos: V2, width: W, height: H) {
        unimplemented!()
    }

    #[inline]
    pub fn line(&self, a: V2, b: V2) {
        unimplemented!()
    }

    #[inline]
    pub fn lines<I: IntoIterator<Item = V2>>(&self, verts: I) {
        self.lines_internal(verts.into_iter().map(|v| [v.x, v.y]))
    }

    #[inline]
    fn lines_internal<I: IntoIterator<Item = [f64; 2]>>(&self, verts: I) {
        unimplemented!()
    }

    #[inline]
    pub fn polygon<I>(&self, verts: I)
    where
        I: Clone + IntoIterator<Item = V2>,
    {
        unimplemented!()
    }
}
