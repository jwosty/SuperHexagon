namespace SuperHexagon

type Difficulty =
  Hexagon | Hexagoner | Hexagonest
  
  member this.hue =
    match this with Hexagon -> 120. | Hexagoner -> 240. | Hexagonest -> 0.