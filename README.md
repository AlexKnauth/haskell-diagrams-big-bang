# haskell-diagrams-big-bang

The `bigBang` function launches a window that keeps track of a "world state" for the window, displays a diagram for the current world state, and updates the world state in response to events like mouse clicks or clock ticks.

Inspired by [`big-bang`](https://docs.racket-lang.org/teachpack/2htdpuniverse.html) 
from How to Design Programs.

Examples:

```haskell
> import Diagrams.Prelude
> import BigBang
-- a square that shrinks over time but grows when you click
> (bigBang
    1.0
    handlers { BigBang.size = dims2D 400 400,
               toDraw = \x -> square (1.0 / x) `atopBG` square 1 # fc white,
               onTick = \x -> Just (x + 0.1),
               onMouseClick = \x _ -> Just (x * 0.5) })
-- dots that appear wherever you click
> (bigBang
    []
    handlers { BigBang.size = dims2D 400 400,
               toDraw = \pts -> mconcat [moveTo p (circle 0.01 # fc black) | p <- pts] `atopBG` square 1 # fc white,
               onMouseClick = \pts pt -> Just (pt : pts) })
```
