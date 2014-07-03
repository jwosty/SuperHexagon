namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions
open System

type Obstacles =
  { obstacles: (int * float) list }
  
  static member CreateDefault() = { obstacles = [] }
  
  member this.CreateRandomizedObstacle rand = Seq.head rand % 6UL |> int, 2.
  
  member this.CheckCollision playerSection = this.obstacles |> List.exists (fun (section, distance) -> distance >|< (1.12, 1.125) && playerSection = section)
  
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

type Game =
  { totalTicks: uint32
    rand: uint64 seq
    playerAngle: int
    obstacles: Obstacles }
  
  static member CreateDefault () =
    { totalTicks = 0u; rand = Seq.unfold (fun x -> Some(x, xorshift x)) <| uint64 DateTime.Now.Ticks;
      playerAngle = 0; obstacles = Obstacles.CreateDefault () }
  
  member this.AddObstaclesIfNeeded obstacles = if this.totalTicks % 50u = 0u then (0, 1.) :: obstacles else obstacles
  
  member this.Update (keyboardState: byte[]) =
    let playerTurn =
      // Keyboard repeat events is unreliable, so just use the current keyboard state
      match keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_LEFT], keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT] with
      | 1uy, 0uy -> -10
      | 0uy, 1uy -> 10
      | _ -> 0
    let playerAngle = this.playerAngle + playerTurn % 360
    let obstacles, rand = this.obstacles.Update this.totalTicks this.rand
    //if obstacles.CheckCollision (float playerAngle * (6. / 360.) |> int) then printfn "GAME OVER"
    { this with
        totalTicks = this.totalTicks + 1u; playerAngle = playerAngle
        obstacles = obstacles; rand = rand }

and PostGame =
  { ticksSurvived: uint32 }
  
  member this.Update (keyboardState: byte[]) = this