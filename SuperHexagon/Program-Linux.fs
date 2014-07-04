module SuperHexagon.Main
open SDL2
open SuperHexagon.HelperFunctions
open System.Diagnostics
open System.Runtime.InteropServices
open System.Threading

let titleUpdateTimer = new Stopwatch()
let frameTimer = new Stopwatch()

let rec runGame (game: SuperHexagon) (gameRenderer: Renderers.Game) =
  frameTimer.Start()
  if not titleUpdateTimer.IsRunning then titleUpdateTimer.Start ()
  let events = pollEvents ()
  
  // Get keyboard events
  let mutable length = 0
  let keyboardPtr = SDL.SDL_GetKeyboardState (&length)
  let keyboardState: byte[] = Array.zeroCreate length
  Marshal.Copy (keyboardPtr, keyboardState, 0, length)
  
  match game.Update events keyboardState with
  | Some(game) ->
      gameRenderer.DrawFrame game
      
      // 20 milliseconds/frame cap
      let elapsed = ticksToMilliseconds frameTimer.ElapsedTicks
      if elapsed < 20. then Thread.Sleep (20. - elapsed |> floor |> int)
      
      if titleUpdateTimer.ElapsedMilliseconds >= 1000L then
        SDL.SDL_SetWindowTitle (gameRenderer.WindowHandle, ("Super Hexagon (" + (1000. / ticksToMilliseconds frameTimer.ElapsedTicks |> int |> string) + " FPS)"))
        titleUpdateTimer.Reset ()
      
      frameTimer.Reset ()
      
      runGame game gameRenderer
  | None -> ()

[<EntryPoint>]
let main args =
  Renderers.Game.Init ()
  let gameRenderer = new Renderers.Game()
  runGame (SuperHexagon.CreateDefault ()) gameRenderer
  0