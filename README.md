# Pollock

## A framework for bad code

Pollock is an idiomatic-enough reimplementation of Processing's ideals and API in Rust, for use to make generative art, small games in game jams, or just as a toy to play with. It tries to put as few restrictions on your code as possible, so writing using it feels as comfortable as writing code in a dynamic language but with less bugs and better performance. Having said that, right now performance in debug mode is pretty terrible because I make heavy use of zero-cost abstractions which heavily rely on optimisations. Performance in release mode is already fantastic, though, and probably can be improved further. One limitation is that I definitely always want to have an immediate-mode interface, although I would love to work out a way to at least partly use retained mode in the backend.

It's still pre-pre-pre-alpha, but it already has some cool features like saving videos and an emulator-style savestate system which can be used to give a workflow similar to hot reloading - making a change, rerunning the program and reloading the state to get back to where you were.

### The Pollock design philosophy

- Worse is better
- Don't make me think
- Performance is king, but usability is God

Pollock is designed to allow quick hacking on code that you don't intend to ever maintain. It's for experiments and mistakes.

If you've used Processing before the API will look familiar. If you've used some big Rust game frameworks the API will probably look totally alien. For a start, you don't define a trait. I chose to use the builder pattern to avoid the problem of having to annotate types everywhere, and to mean that you need to understand less of Rust to be able to write something, even if it's only something simple, in Rust.

```rust
extern crate pollock;

use pollock::*;

fn main() {
    Pollock::new()
        .setup(|p| {
            p.size = size;
            p.background = rgb(255, 255, 255);
            p.fill = Fill::none();
            p.stroke = Stroke::new(rgb(0, 0, 0), 3);
        })
        .draw(|p| {
            // Example spinning square
            let mut p = p.push();
            p.rotate(p.frame_count as f64 / 100.);
            p.translate(v2(p.width(), p.height()));
            p.square(v2(0, 0), 50);
        })
        .run();
}
```

`v2` creates a vector, I think that the rest is self-explanatory. I use the `Vector2` type from `nalgebra` for my vector type, although this might change in the future since the error messages when using this type are utterly indefensible. It does allow you to do really nice stuff out of the box that is really unwieldy in Processing. For example, you can just add and subtract vectors, multiply them by scalars and all other nice things that are useful in a context where you're working with 2D graphics. I don't currently expose many of `nalgebra`'s features but as I get more feedback on the design of the API I may add more.

Additionally, I support creating lines from an iterator of points, and in fact everything that can work with iterators does work with iterators. This does mean that it's hard to do a line from a fixed array, but I already have a `fixed_size_iter` macro internally that I might expose (under a nicer name, of course).

### How you can help

Use it! Tell me where features are missing and whether it's actually as easy to use as I hope it is. I do worry that my heavy use of generics and `Deref` might make it a little difficult to read the documentation, but this can probably be helped with careful use of type aliases.

Here's an example gif from a Processing sketch that I ported to Pollock:

![](./resources/psychedelics.gif)

Future features:

- Images
- Sound input (from a file and/or from the microphone) for visualisations
- Mouse input
- Module system to add extra functionality to `PollockState` that looks like it was implemented in the core library. I have some ideas of how to use the type system to achieve this but I haven't put anything concrete down yet.
- More inbuilt maths helpers
