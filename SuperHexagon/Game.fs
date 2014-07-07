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
  
  member this.Update keyboard rand timeFactor =
    if this.gameTime >= this.duration then
      if Seq.head rand % 4UL = 0UL
      then { this with gameTime = 0.; clockwise = not this.clockwise; speed = 4.; duration = 50. }, Seq.skip 1 rand
      else { this with gameTime = 0.; clockwise = not this.clockwise; speed = 1.; duration = 500. }, Seq.skip 1 rand
    else { this with gameTime = this.gameTime + timeFactor; screenAngle = this.screenAngle + ((if this.clockwise then timeFactor else -timeFactor) * this.speed) }, rand

type Game =
  { gameTime: float; rand: uint64 seq; difficulty: Difficulty
    playerAngle: float; rotation: GameRotation; obstacles: Obstacles }
  
  static member CreateDefault difficulty =
    let rand = Seq.unfold (fun x -> Some(x, xorshift x)) <| uint64 DateTime.Now.Ticks
    { gameTime = 0.; rand = Seq.skip 1 rand; difficulty = difficulty
      playerAngle = float (Seq.head rand) % 360.; rotation = GameRotation.CreateDefault ()
      obstacles = Obstacles.CreateDefault () }
  
  member this.hue = this.difficulty.hue + (60.*(abs(((this.gameTime/256.+1.)%4.)-2.)-1.))
  
  interface IGameScreen with
    member this.Update lastKeyboardState keyboard timeFactor =
      let playerTurn =
        match keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
        | 1uy, 0uy -> -7.5 * timeFactor
        | 0uy, 1uy -> 7.5 * timeFactor
        | _ -> 0.
      let playerAngle = this.playerAngle + playerTurn
      let obstacles, rand = this.obstacles.Update timeFactor this.rand
      let rotation, rand = this.rotation.Update keyboard rand timeFactor
      if this.obstacles.CollidingWithPlayer (angleToHexagonFace (float playerAngle)) then
        Transition.CreateDefault this ({ MainMenu.CreateDefault () with selectedDifficulty = this.difficulty }) 25. :> _
      else
        { this with
            gameTime = this.gameTime + timeFactor; playerAngle = playerAngle; rotation = rotation
            obstacles = obstacles; rand = rand } :> _

and MainMenu =
  { gameTime: float; screenAngle: float; selectedDifficulty: Difficulty }
  
  static member CreateDefault () = { gameTime = 0.; screenAngle = 0.; selectedDifficulty = Difficulty.Hexagon }
  
  member this.hue = this.selectedDifficulty.hue + (45.*(abs(((this.gameTime/512.+1.)%4.)-2.)-1.))
  
  interface IGameScreen with
    member this.Update lastKeyboardState keyboardState timeFactor =
      let inline buttonJustPressed button = buttonJustPressed lastKeyboardState keyboardState button
      if keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_SPACE] = 1uy
      then upcast (Transition.CreateDefault this (Game.CreateDefault this.selectedDifficulty) 15.)
      else
        let this = { this with gameTime = this.gameTime + timeFactor; screenAngle = this.screenAngle - (0.25 * timeFactor) }
        let l,r = int SDL.SDL_Scancode.SDL_SCANCODE_LEFT, int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT
        match buttonJustPressed SDL.SDL_Scancode.SDL_SCANCODE_LEFT, buttonJustPressed SDL.SDL_Scancode.SDL_SCANCODE_RIGHT with
        | false, true ->
            let next = { this with selectedDifficulty = match this.selectedDifficulty with Hexagon -> Hexagoner | Hexagoner -> Hexagonest | Hexagonest -> Hexagon }
            Transition.CreateDefault this next 25. :> _
        | true, false ->
            let next = { this with selectedDifficulty = match this.selectedDifficulty with Hexagon -> Hexagonest | Hexagoner -> Hexagon | Hexagonest -> Hexagoner }
            Transition.CreateDefault this next 25. :> _
        | _ -> upcast this