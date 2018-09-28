# Pollock

## A framework for bad code

Pollock is an idiomatic-enough reimplementation of Processing's ideals and API in Rust, for use to make generative art, small games in game jams, or just as a toy to play with. It tries to put as few restrictions on your code as possible, so writing using it feels as comfortable as writing code in a dynamic language but with less bugs and better performance. Having said that, right now performance in debug mode is pretty terrible because I make heavy use of zero-cost abstractions which heavily rely on optimisations. Performance in release mode is already fantastic, though, and probably can be improved further. One limitation is that I definitely always want to have an immediate-mode interface, although I would love to work out a way to at least partly use retained mode in the backend. I already batch up all calls into a single draw call.

It's still pre-pre-pre-alpha, but it already has some cool features like saving videos and an emulator-style savestate system which can be used to give a workflow similar to hot reloading - making a change, rerunning the program and reloading the state to get back to where you were.

### The Pollock design philosophy

- Worse is better
- Don't make me think
- Performance is king, but usability is God

Pollock is designed to allow quick hacking on code that you don't intend to ever maintain. It's for experiments and mistakes.

If you've used Processing before the API will look familiar. If you've used some popular Rust game frameworks the API will probably look totally alien. For a start, you don't define a trait. I chose to use the builder pattern to avoid the problem of having to annotate types everywhere, and to mean that you need to understand less of Rust to be able to write something, even if it's only something simple, in Rust.

```rust,no_run
extern crate pollock;

use pollock::*;

fn main() {
    Pollock::setup(|p| {
        p.size = (600, 480);
        // Like 
        p.background = Fill::default();
        p.fill = Fill::none();
        p.stroke = Stroke::new(rgb(0, 0, 0), 3);
    }).draw(|p| {
        // Example spinning square
        let mut p = p.push();
        let size = 50;
        let (w, h) = p.size;
        let frame_count = p.frame_count as f64;

        p.rotate(frame_count / 100.);
        p.translate(v2(w, h) / 2.);
        p.rect(v2(-size / 2, -size / 2), size, size);
    }).run();
}
```

`v2` creates a vector, `push` is a Rustified version of Processing's `pushMatrix` (the changes are popped when the new `p` goes out of scope). I think that the rest is self-explanatory. I use the `Vector2` type from `nalgebra` for my vector type, although this might change in the future since the error messages when using this type are utterly indefensible. It does allow you to do really nice stuff out of the box that is really unwieldy in Processing. For example, you can just add and subtract vectors, multiply them by scalars and all other nice things that are useful in a context where you're working with 2D graphics. I don't currently expose many of `nalgebra`'s features but as I get more feedback on the design of the API I may add more.

Something that I don't show off in this example or the example below is that I don't have an equivalent of `startShape` and `endShape`, everything's just done with iterators. You can build both polygons and lines (with joins done automatically) from an iterator over `V2`s (i.e. 2D vectors). This is very performant, arguably more ergonomic, and doesn't have the possibility of accidentally forgetting to call `endShape`.

### Handling mutable state

The way you do mutable state in Python is to just use global variables. This works in Pollock, too (although they'd be mutable variables in the scope of the function that you're calling `Pollock::run` in), but Pollock additionally supports the concept of a state struct. A benefit to this is that you can get savestates for free, but it does add an additional type annotation burden so for simple projects you can just use global variables.

Using a state struct is as easy as just returning it from the `setup` function and then using `p.state` to access it in your main loop. For example, for a simple sketch that shows some bouncing balls it might look something like this:

```rust,no_run
extern crate pollock;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use pollock::*;

#[derive(Serialize, Deserialize)]
struct State {
    balls: Vec<(V2, V2)>,
}

fn main() {
    let radius = 10.;

    Pollock::setup(|p| {
        p.size = (600, 480);
        p.background = Fill::default();
        p.stroke = Stroke::none();
        // We add alpha so we can see where the balls intersect
        // (also so I can show off the `rgba` function.
        p.fill = Fill::new(rgba(0, 0, 0, 100));

        State {
            balls: (0..10)
                .map(|_| {
                    (
                        v2(
                            p.random_range(0., p.width() as f64),
                            p.random_range(0., p.height() as f64),
                        ),
                        v2(
                            if p.random() { -1 } else { 1 },
                            if p.random() { -1 } else { 1 },
                        ),
                    )
                }).collect(),
        }
    }).draw(|p| {
        let (width, height) = p.size;
        // A limitation of Rust's disjointness analysis means that
        // we have to loop twice here.
        for (pos, vel) in &mut p.state.balls {
            if pos.x < radius {
                vel.x = 1.
            } else if pos.x > width as f64 - radius {
                vel.x = -1.
            }

            if pos.y < radius {
                vel.y = 1.
            } else if pos.y > height as f64 - radius {
                vel.y = -1.
            }

            *pos += *vel * 1.;
        }

        for (pos, _) in p.state.balls.iter() {
            p.circle(*pos, radius);
        }
    })
    .on_key_down(Key::O, |p| p.save_state("state"))
    .on_key_down(Key::P, |p| p.load_state("state"))
    .on_key_down(Key::Space, |p| p.paused = !p.paused)
    .run();
}
```

You can see that it's very terse and (in my opinion) very readable, although you would have to know that returning state from `setup` makes it accessible in `p.state` which is admittedly fairly magic.

In case you're wondering, you can access `p.state` in `setup`, but it just has the type `()`.

### How you can help

Use it! Tell me where features are missing and whether it's actually as easy to use as I hope it is. I do worry that my heavy use of generics and `Deref` might make it a little difficult to read the documentation, but this can probably be helped with careful use of type aliases. I want to do some work to port Processing tutorials to Pollock to find out where common operations aren't supported.

Here's an example gif from a Processing sketch that I ported to Pollock:

![](./resources/psychedelics.gif)

Future features:

- Images
- Text
- Sound input (from a file and/or from the microphone) for visualisations
- Mouse input
- Module system to add extra functionality to `PollockState` that looks like it was implemented in the core library. I have some ideas of how to use the type system to achieve this but I haven't put anything concrete down yet.
- More inbuilt maths helpers
