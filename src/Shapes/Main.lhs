# The Gelatin Tutorial, Part 2 - Shapes

```haskell
module Main where

import Control.Monad.Except (runExceptT)
import Gelatin.SDL2

main :: IO ()
main = do
  Right (SDL2Backends colorBackend _) <-
    runExceptT $ startupSDL2Backends 300 300 "Shapes!" False

  let circlePic :: ColorPicture ()
      circlePic = do
        setStroke [StrokeWidth 10, StrokeFeather 3]
        setGeometry $ line $ mapVertices (\v -> (v, white)) (arc 100 100 0 (2 * pi))

  (_, rendering) <- compilePicture colorBackend circlePic

  let loop = do
        events <- getEvents colorBackend
        clearWindow colorBackend
        snd rendering [move 150 150]
        updateWindow colorBackend
        loop

  loop
```
