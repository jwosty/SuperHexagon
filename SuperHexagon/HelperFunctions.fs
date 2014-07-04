module SuperHexagon.HelperFunctions
open SDL2
open System
open System.Diagnostics
open System.Runtime.InteropServices

let ptrToStructure<'T when 'T : struct> ptr = Marshal.PtrToStructure (ptr, typeof<'T>) :?> 'T

let rec pollEvents () =
  let event = ref Unchecked.defaultof<_>
  if (SDL.SDL_PollEvent event) = 1 then
    !event :: pollEvents ()
  else []

let eventExists eventType = List.exists (fun (event: SDL.SDL_Event) -> event.``type`` = eventType)
let quitRequested events (keyboardState: byte[]) =
  eventExists SDL.SDL_EventType.SDL_QUIT events || keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_ESCAPE] = 1uy ||
  ((keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_LGUI] = 1uy || keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_RGUI] = 1uy) && keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_Q] = 1uy)

// Execute `dangerous` but guarentee that `important` gets executed even if an error is thrown.
let guarentee important dangerous =
  let e = try dangerous (); None with | e -> Some(e)
  important ()
  match e with Some(e) -> raise e | None -> ()

let ticksToMilliseconds (ticks: int64) = ((float ticks) / (float Stopwatch.Frequency)) * 1000.
let (>|<) v (min, max) = v >= min && v <= max
let toRadians degrees = degrees * (Math.PI / 180.)
let toDegrees radians = radians * (180. / Math.PI)
let inline wrap max x = ((x % max) + max) % max

let unitHexagonVertices =
  seq {
    for i in -2. .. 4. ->
      let p = Math.PI * 2. * ((i + 0.5 |> wrap 6.) / 6.)
      cos p, sin p }

let xorshift x =
  let x = x ^^^ (x >>> 12)
  let x = x ^^^ (x <<< 25)
  let x = x ^^^ (x >>> 27)
  x * 2685821657736338717UL

let splitSeq seq = Seq.head seq, Seq.skip 1 seq

let playerCollidingWith playerSection (obstacleSection: int, obstacleDistance) = (playerSection = obstacleSection) && obstacleDistance >|< (0.12, 0.15)
let playerColliding playerSection obstacles = obstacles |> List.exists (playerCollidingWith playerSection)
let angleToHexagonFace degrees = degrees * (6. / 360.) |> wrap 6. |> int