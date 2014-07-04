namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions
open System

type Obstacle =
  { section: int; distance: float }
  
  member this.Update () =
    let distance = this.distance - 0.01
    if distance > 0. then Some({ this with distance = distance }) else None
  
  member this.CollidingWithPlayer playerSection = (playerSection = this.section) && this.distance >|< (0.12, 0.14)

module Obstacles =
  // Configurations in easy mode
  let easyGroups = [ [  2;3;4;5;6]; [1;2;3;  5;6]; [1;3;5]; [2;4;6] ] |> List.map (List.map (fun s -> { section = s; distance = 2.}))
  
  let spawnGroup rand = easyGroups.[Seq.head rand |> int |> wrap easyGroups.Length]
  
  let update obstacles totalTicks rand =
    obstacles
      |> List.choose (fun (o: Obstacle) -> o.Update ())
      |> (fun obstacles ->
          if totalTicks % 50u = 0u
          then obstacles @ spawnGroup rand, Seq.skip 1 rand
          else obstacles, rand)
  
  let collidingWithPlayer playerSection obstacles = List.exists (fun (o: Obstacle) -> o.CollidingWithPlayer playerSection) obstacles

type IGameScreen =
  abstract Update: byte[] -> IGameScreen

type Transition =
  { start: IGameScreen; finish: IGameScreen
    finishTicks: int; progress: int }
  
  static member CreateDefault start finish finishTicks =
    { start = start; finish = finish
      finishTicks = finishTicks; progress = 0 }
  
  interface IGameScreen with
    member this.Update keyboard =
      if (this.progress + 1) >= this.finishTicks
      then this.finish
      else upcast { this with progress = this.progress + 1 }

type GameRotation =
  { clockwise: bool; speed: float; duration: int; progress: int }
  
  static member CreateDefault () = { clockwise = true; speed = 1.; duration = 500; progress = 0 }
  
  member this.Delta = if this.clockwise then this.speed else -this.speed
  
  member this.Update keyboard =
    if this.progress >= this.duration
    then { this with clockwise = not this.clockwise; progress = 0 }
    else { this with progress = this.progress + 1 }

type Game =
  { totalTicks: uint32
    rand: uint64 seq
    playerAngle: int
    rotation: GameRotation
    screenAngle: float
    obstacles: Obstacle list }
  
  static member CreateDefault () =
    let rand = Seq.unfold (fun x -> Some(x, xorshift x)) <| uint64 DateTime.Now.Ticks
    { totalTicks = 0u; rand = Seq.skip 1 rand;
      playerAngle = int (Seq.head rand) % 360; rotation = GameRotation.CreateDefault (); screenAngle = 0.; obstacles = [] }
  
  member this.AddObstaclesIfNeeded obstacles = if this.totalTicks % 50u = 0u then (0, 1.) :: obstacles else obstacles
  
  interface IGameScreen with
    member this.Update keyboard =
      let playerTurn =
        // Keyboard repeat events are unreliable, so just use the current keyboard state
        match keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboard.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
        | 1uy, 0uy -> -10
        | 0uy, 1uy -> 10
        | _ -> 0
      let playerAngle = this.playerAngle + playerTurn
      let obstacles, rand = Obstacles.update this.obstacles this.totalTicks this.rand
      let rotation = this.rotation.Update keyboard
      if Obstacles.collidingWithPlayer (angleToHexagonFace (float playerAngle)) obstacles then
        Transition.CreateDefault this (PostGame.CreateDefault ()) 25 :> _
      else
        { this with
            totalTicks = this.totalTicks + 1u; playerAngle = playerAngle
            rotation = rotation; screenAngle = this.screenAngle + rotation.Delta
            obstacles = obstacles; rand = rand } :> _

and PostGame =
  { screenAngle: float }
  
  static member CreateDefault () = { screenAngle = 0. }
  
  interface IGameScreen with
    member this.Update keyboardState =
      if keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_SPACE] = 1uy
      then upcast (Transition.CreateDefault this (Game.CreateDefault ()) 15)
      else upcast { this with screenAngle = this.screenAngle - 0.25 }