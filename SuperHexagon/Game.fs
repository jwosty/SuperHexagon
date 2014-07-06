﻿namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions
open System

type Obstacle =
  { section: int; distance: float }
  
  member this.Update () =
    let distance = this.distance - 0.01
    if distance > 0. then Some({ this with distance = distance }) else None
  
  member this.CollidingWithPlayer playerSection = (playerSection = (wrap 6 this.section)) && this.distance >|< (0.12, 0.14)

type Obstacles =
  { items: Obstacle list; timeUntilGroupSpawn: float }
  
  static member CreateDefault () = { items = []; timeUntilGroupSpawn = 0. }
  
  // Configurations in easy mode
  static member EasyGroups =
    [ [  1;2;3;4;5]; [0;1;2;  4;5]
      [  1;2;3;4;5]; [0;1;2;  4;5]
      [0;2;4];       [1;3;5]
      [0;1;3;4];     [1;2;4;5];     [2;3;5;0] ]
      |> List.map (List.map (fun s -> { section = s; distance = 2.}))
  
  static member SpawnGroup rand = Obstacles.EasyGroups.[Seq.head rand |> int |> wrap Obstacles.EasyGroups.Length]
  
  member this.Update timeFactor rand =
    let items, rand =
      this.items
        |> List.choose (fun (o: Obstacle) -> o.Update ())
        |> (fun obstacles ->
            if this.timeUntilGroupSpawn <= 0.
            then obstacles @ Obstacles.SpawnGroup rand, Seq.skip 1 rand
            else obstacles, rand)
    { this with
        items = items;
        timeUntilGroupSpawn = (if this.timeUntilGroupSpawn > 0. then this.timeUntilGroupSpawn - timeFactor else 50.) }, rand
  
  member this.CollidingWithPlayer playerSection = List.exists (fun (o: Obstacle) -> o.CollidingWithPlayer playerSection) this.items

type IGameScreen =
  abstract Update: keyboardState:byte[] -> timeFactor:float -> IGameScreen

type Transition =
  { start: IGameScreen; finish: IGameScreen
    gameTimeDuration: float; gameTime: float }
  
  static member CreateDefault start finish totalDuration =
    { start = start; finish = finish
      gameTimeDuration = totalDuration; gameTime = 0. }
  
  interface IGameScreen with
    member this.Update keyboard timeFactor =
      if (this.gameTime + 1.) >= this.gameTimeDuration
      then this.finish
      else upcast { this with gameTime = this.gameTime + timeFactor }

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
  { screenAngle: float; hue: float }
  
  static member CreateDefault () = { screenAngle = 0.; hue = 120. }
  
  interface IGameScreen with
    member this.Update keyboardState timeFactor =
      if keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_SPACE] = 1uy
      then upcast (Transition.CreateDefault this (Game.CreateDefault ()) 15.)
      else upcast { this with screenAngle = this.screenAngle - (0.25 * timeFactor) }