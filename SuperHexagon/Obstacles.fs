namespace SuperHexagon
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