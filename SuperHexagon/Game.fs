namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions
open System

type Difficulty =
  Hexagon | Hexagoner | Hexagonest
  
  member this.hue =
    match this with Hexagon -> 120. | Hexagoner -> 240. | Hexagonest -> 0.

type GameRotation =
  { screenAngle: float; clockwise: bool; speed: float
    duration: float; gameTime: float }
  
  static member CreateDefault () =
    { screenAngle = 0.; clockwise = true; speed = 1.
      duration = 500.; gameTime = 0. }
  
  member this.Update keyboard timeFactor =
    if this.gameTime >= this.duration
    then { this with clockwise = not this.clockwise; gameTime = 0. }
    else { this with gameTime = this.gameTime + timeFactor; screenAngle = this.screenAngle + ((if this.clockwise then timeFactor else -timeFactor) * this.speed) }

type Game =
  { gameTime: float; rand: uint64 seq; playerAngle: float
    rotation: GameRotation; hue: float; obstacles: Obstacles }
  
  static member CreateDefault () =
    let rand = Seq.unfold (fun x -> Some(x, xorshift x)) <| uint64 DateTime.Now.Ticks
    { gameTime = 0.; rand = Seq.skip 1 rand; playerAngle = float (Seq.head rand) % 360.;
      rotation = GameRotation.CreateDefault (); hue = 240.; obstacles = Obstacles.CreateDefault () }
  
  member this.AddObstaclesIfNeeded obstacles = if this.gameTime % 50. = 0. then (0, 1.) :: obstacles else obstacles
  
  interface IGameScreen with
    member this.Update keyboard timeFactor =
      let playerTurn =
        // Keyboard repeat events are unreliable, so just use the current keyboard state
        match keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
        | 1uy, 0uy -> -7.5 * timeFactor
        | 0uy, 1uy -> 7.5 * timeFactor
        | _ -> 0.
      let playerAngle = this.playerAngle + playerTurn
      let obstacles, rand = this.obstacles.Update timeFactor this.rand
      let rotation = this.rotation.Update keyboard timeFactor
      if this.obstacles.CollidingWithPlayer (angleToHexagonFace (float playerAngle)) then
        Transition.CreateDefault this (MainMenu.CreateDefault ()) 25. :> _
      else
        { this with
            gameTime = this.gameTime + timeFactor; playerAngle = playerAngle; rotation = rotation
            hue = wrap 360. (this.hue + (0.25 * timeFactor)); obstacles = obstacles; rand = rand } :> _

and MainMenu =
  { screenAngle: float; selectedDifficulty: Difficulty }
  
  static member CreateDefault () = { screenAngle = 0.; selectedDifficulty = Difficulty.Hexagon }
  
  interface IGameScreen with
    member this.Update keyboardState timeFactor =
      if keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_SPACE] = 1uy
      then upcast (Transition.CreateDefault this (Game.CreateDefault ()) 15.)
      else
        let this = { this with screenAngle = this.screenAngle - (0.25 * timeFactor) }
        match keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
        | 1uy, 0uy -> upcast { this with selectedDifficulty = match this.selectedDifficulty with Hexagon -> Hexagoner | Hexagoner -> Hexagonest | Hexagonest -> Hexagon }
        | 0uy, 1uy -> upcast { this with selectedDifficulty = match this.selectedDifficulty with Hexagon -> Hexagonest | Hexagoner -> Hexagon | Hexagonest -> Hexagoner }
        | _ -> upcast this