# The Gelatin Tutorial
This tutorial was created as the goal of my time at
[BayHac 2018](https://wiki.haskell.org/BayHac2018). This file is written in
[literate haskell](https://wiki.haskell.org/Literate_programming) so we'll start
by importing gelatin-sdl2 and a helper.


```haskell
module Main where

import Control.Monad.Except (runExceptT)
import Gelatin.SDL2
```

Gelatin is a rendering system with multiple backends so the first step is
to obtain a concrete backend. This operation creates a window with the given width,
height and title. We also pass in whether or not we'd like to use a high-dpi
framebuffer, which in this example we don't need. This can fail in the case that
SDL cannot create a window or SDL cannot initialize the OpenGL context. For this
reason we run it in the [ExceptT](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#g:2)
transformer.

```haskell
main :: IO ()
main = do
  Right (SDL2Backends colorBackend _) <-
    runExceptT $ startupSDL2Backends 300 300 "Hello Triangle" False
```

Now that we have our backend we can create and compile a picture. A picture is
a collection of geometry made up of points in space and color. In math these are
known as vertices and in shaders these are known as "attributes". Here our
vertices are of type `(V2 Float, V4 Float)`, representing a mapping of a point in
two dimensional space to a color. We'll use these vertices to define a triangle.

```haskell
  let helloTriangle = triangles $ tri (V2 0 0    , red)
                                      (V2 100 0  , green)
                                      (V2 100 100, blue)
```

Pure geometry can be interpreted as many things, so along with defining three
vertices we wrap the vertices in `triangles`. This tells gelatin to render our
three points as a triangle, as opposed to a bezier or a line.

Now that we have our geometry we can build our picture. In this simple example
we don't need to do anything more than set that geometry in our picture.

```haskell
  let helloPicture = setGeometry helloGeometry
```

Next our picture will be compiled and sent to the GPU. What we get back is a ``

```haskell
  (_, rendering) <- compilePicture colorBackend helloPicture
```

```haskell
  let loop = do
        events <- getEvents colorBackend
        clearWindow colorBackend
        snd rendering [move 100 100]
        updateWindow colorBackend

  loop
```
