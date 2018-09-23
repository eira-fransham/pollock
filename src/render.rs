pub struct DrawState<'a> {
    context: Context,
    gl: &'a mut GlGraphics,
}

impl<'a, 'b, S> ExtendedState<'a, S, DrawState<'b>> {
    fn fill_color(&self) -> [f32; 4] {
        [
            self.fill.color.color.red as f32 / 255.0,
            self.fill.color.color.green as f32 / 255.0,
            self.fill.color.color.blue as f32 / 255.0,
            self.fill.color.alpha as f32 / 255.0,
        ]
    }

    fn stroke_color(&self) -> [f32; 4] {
        [
            self.stroke.color.color.red as f32 / 255.0,
            self.stroke.color.color.green as f32 / 255.0,
            self.stroke.color.color.blue as f32 / 255.0,
            self.stroke.color.alpha as f32 / 255.0,
        ]
    }

    pub fn circle<R: Into<f64>>(&self, pos: V2, rad: R) {
        let rad = rad.into();
        self.ellipse(pos, rad, rad)
    }

    pub fn ellipse<RX: Into<f64>, RY: Into<f64>>(&self, pos: V2, rad_x: RX, rad_y: RY) {
        use std::f64::consts;

        let (rad_x, rad_y) = (rad_x.into(), rad_y.into());
        let perimeter = consts::PI * (rad_x * rad_x + rad_y * rad_y).sqrt() / consts::SQRT_2;

        let inner = &mut *self.inner.borrow_mut();
        let shape = ellipse::Ellipse {
            color: self.fill_color(),
            border: if self.stroke.is_none() {
                None
            } else {
                Some(ellipse::Border {
                    color: self.stroke_color(),
                    radius: self.stroke.thickness,
                })
            },
            resolution: (perimeter / 2.) as _,
        };

        inner.gl.ellipse(
            &shape,
            [pos.x - rad_x, pos.y - rad_y, rad_x * 2., rad_y * 2.],
            &Default::default(),
            inner.context.transform,
        );
    }

    pub fn rect<W: Into<f64>, H: Into<f64>>(&self, pos: V2, width: W, height: H) {
        let inner = &mut *self.inner.borrow_mut();
        let shape = rectangle::Rectangle {
            color: self.fill_color(),
            shape: rectangle::Shape::Square,
            border: if self.stroke.is_none() {
                None
            } else {
                Some(rectangle::Border {
                    color: self.stroke_color(),
                    radius: self.stroke.thickness,
                })
            },
        };

        inner.gl.rectangle(
            &shape,
            [pos.x, pos.y, width.into(), height.into()],
            &Default::default(),
            inner.context.transform,
        );
    }

    #[inline]
    pub fn line(&self, a: V2, b: V2) {
        let inner = &mut *self.inner.borrow_mut();
        inner.gl.line(
            &line::Line {
                color: self.stroke_color(),
                radius: self.stroke.thickness,
                shape: line::Shape::Square,
            },
            [a.x, a.y, b.x, b.y],
            &Default::default(),
            inner.context.transform,
        );
    }

    #[inline]
    pub fn lines<I: IntoIterator<Item = V2>>(&self, verts: I) {
        self.lines_internal(verts.into_iter().map(|v| [v.x, v.y]))
    }

    #[inline]
    fn lines_internal<I: IntoIterator<Item = [f64; 2]>>(&self, verts: I) {
        let mut verts = verts.into_iter();
        if let Some(mut last) = verts.next() {
            for cur in verts {
                self.line(v2(last[0], last[1]), v2(cur[0], cur[1]));
                last = cur;
            }
        }
    }

    #[inline]
    pub fn polygon<I>(&self, verts: I)
    where
        I: Clone + IntoIterator<Item = V2>,
    {
        // TODO: Have some specialised version that is faster for the
        //       common case of a non-procedural polygon
        let poly: Vec<_> = verts.into_iter().map(|v| [v.x, v.y]).collect();
        let inner = &mut *self.inner.borrow_mut();
        inner.gl.polygon(
            &polygon::Polygon {
                color: self.fill_color(),
            },
            &poly,
            &Default::default(),
            inner.context.transform,
        );

        if !self.stroke.is_none() {
            let first = poly.first().map(|&a| a);
            self.lines_internal(poly.into_iter().chain(first));
        }
    }
}
