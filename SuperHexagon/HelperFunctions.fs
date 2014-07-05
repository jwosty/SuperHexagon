module SuperHexagon.HelperFunctions
open OpenTK.Graphics.OpenGL
open SDL2
open System
open System.Diagnostics
open System.Runtime.InteropServices

let ptrToStructure<'T when 'T : struct> ptr = Marshal.PtrToStructure (ptr, typeof<'T>) :?> 'T

let sdlColor (r, g, b, a) =
  let mutable c = new SDL.SDL_Color()
  c.r <- r; c.g <- g; c.b <- b; c.a <- a
  c

let rec pollEvents () =
  let event = ref Unchecked.defaultof<_>
  if (SDL.SDL_PollEvent event) = 1 then
    !event :: pollEvents ()
  else []

let eventExists eventType = List.exists (fun (event: SDL.SDL_Event) -> event.``type`` = eventType)
let quitRequested events (keyboardState: byte[]) =
  eventExists SDL.SDL_EventType.SDL_QUIT events || keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_ESCAPE] = 1uy ||
  ((keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_LGUI] = 1uy || keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_RGUI] = 1uy) && keyboardState.[int SDL.SDL_Scancode.SDL_SCANCODE_Q] = 1uy)

/// Execute `dangerous` but guarentee that `important` gets executed even if an error is thrown.
let guarentee important dangerous =
  let e = try dangerous (); None with | e -> Some(e)
  important ()
  match e with Some(e) -> raise e | None -> ()

let ticksToMilliseconds (ticks: int64) = ((float ticks) / (float Stopwatch.Frequency)) * 1000.
let (>|<) v (min, max) = v >= min && v <= max
let toRadians degrees = degrees * (Math.PI / 180.)
let toDegrees radians = radians * (180. / Math.PI)
let inline wrap max x = ((x % max) + max) % max
/// Linear interpolation function, where applying 0 will result in `a` and 1 in `b`
let lerp (a, b) (v: float) = a + ((b - a) * v)

let unitHexagonVertices =
  seq {
    for i in -2. .. 4. ->
      let p = Math.PI * 2. * ((i + 0.5 |> wrap 6.) / 6.)
      cos p, sin p }

// Reverse triangle wave, where i is the interval
let rtri i x = floor (x / i) - (x / i) + 1.

let xorshift x =
  let x = x ^^^ (x >>> 12)
  let x = x ^^^ (x <<< 25)
  let x = x ^^^ (x >>> 27)
  x * 2685821657736338717UL

let splitSeq seq = Seq.head seq, Seq.skip 1 seq

let angleToHexagonFace degrees = degrees * (6. / 360.) |> wrap 6. |> int

let hsv2rgb (h, s, v) =
  let c = v * s
  let h' = (wrap 360. h) / 60.
  let x = c * (1. - abs (h' % 2. - 1.))
  let r1, g1, b1 =
    if      0. <= h' && h' < 1. then c,  x,  0.
    else if 1. <= h' && h' < 2. then x,  c,  0.
    else if 2. <= h' && h' < 3. then 0., c,  x
    else if 3. <= h' && h' < 4. then 0., x,  c
    else if 4. <= h' && h' < 5. then x,  0., c
    else (* 5<h<6 *)                 c,  0., x
  let m = v - c
  r1 + m, g1 + m, b1 + m // = r, g, b