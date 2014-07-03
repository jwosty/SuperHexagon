namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions
open System

type Obstacles =
  { obstacles: (int * float) list }
  
  static member CreateDefault() = { obstacles = [] }
  
  member this.CreateRandomizedObstacle rand = Seq.head rand % 6UL |> int, 2.
  
  member this.Update totalTicks rand =
    let obstacles, rand =
      this.obstacles
        |> List.map (fun (section, distance) -> section, distance - 0.005)
        |> List.filter (fun (_, distance) -> distance > 0.)
        |> (fun obstacles ->
            if totalTicks % 50u = 0u
            then this.CreateRandomizedObstacle rand :: this.CreateRandomizedObstacle (Seq.skip 1 rand) :: obstacles, Seq.skip 2 rand
            else obstacles, rand)
    { this with obstacles = obstacles }, rand

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

type Game =
  { totalTicks: uint32
    rand: uint64 seq
    playerAngle: int
    obstacles: Obstacles }
  
  static member CreateDefault () =
    { totalTicks = 0u; rand = Seq.unfold (fun x -> Some(x, xorshift x)) <| uint64 DateTime.Now.Ticks;
      playerAngle = 0; obstacles = Obstacles.CreateDefault () }
  
  member this.AddObstaclesIfNeeded obstacles = if this.totalTicks % 50u = 0u then (0, 1.) :: obstacles else obstacles
  
  interface IGameScreen with
    member this.Update (keyboardState: byte[]) =
      let playerTurn =
        // Keyboard repeat events are unreliable, so just use the current keyboard state
        match keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
        | 1uy, 0uy -> -10
        | 0uy, 1uy -> 10
        | _ -> 0
      let playerAngle = this.playerAngle + playerTurn % 360
      let obstacles, rand = this.obstacles.Update this.totalTicks this.rand
      if playerColliding (angleToHexagonFace (float playerAngle)) obstacles.obstacles then
        Transition.CreateDefault this (PostGame.CreateDefault this.totalTicks) 500 :> _
      else
        { this with
            totalTicks = this.totalTicks + 1u; playerAngle = playerAngle
            obstacles = obstacles; rand = rand } :> _

and PostGame =
  { ticksSurvived: uint32 }
  
  static member CreateDefault ticksSurvived = { ticksSurvived = ticksSurvived }
  
  interface IGameScreen with
    member this.Update keyboardState = upcast this