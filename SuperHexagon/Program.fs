namespace SuperHexagon
#if MONOMAC
open MonoMac.AppKit
open MonoMac.Foundation
#endif
open SDL2
open SuperHexagon.HelperFunctions
open System.Diagnostics
open System.Runtime.InteropServices
open System.Threading

#if MONOMAC
type AppDelegate() =
  inherit NSApplicationDelegate()
  let titleUpdateTimer = new Stopwatch()
  let frameTimer = new Stopwatch()
  
  override this.ApplicationShouldTerminateAfterLastWindowClosed(sender) =
    true
  
  member this.RunGame (game: SuperHexagon) (gameRenderer: Renderers.Game) lastTimeFactor =
    let runGame = this.RunGame
#else
module GameLoop =
  let titleUpdateTimer = new Stopwatch()
  let frameTimer = new Stopwatch()
  
  let rec runGame (game: SuperHexagon) (gameRenderer: Renderers.Game) lastTimeFactor =
#endif
    frameTimer.Start()
    if not titleUpdateTimer.IsRunning then titleUpdateTimer.Start ()
    let events = pollEvents ()
    
    // Get keyboard events
    let mutable length = 0
    let keyboardPtr = SDL.SDL_GetKeyboardState (&length)
    let keyboardState: byte[] = Array.zeroCreate length
    Marshal.Copy (keyboardPtr, keyboardState, 0, length)
    
    match game.Update events keyboardState lastTimeFactor with
    | Some(game) ->
        gameRenderer.DrawFrame game
        
        // 20 milliseconds/frame cap
        let elapsed = ticksToMilliseconds frameTimer.ElapsedTicks
        if elapsed < 22. then Thread.Sleep (22. - elapsed |> floor |> int)
        
        if titleUpdateTimer.ElapsedMilliseconds >= 1000L then
          SDL.SDL_SetWindowTitle (gameRenderer.WindowHandle, ("Super Hexagon (" + (1000. / ticksToMilliseconds frameTimer.ElapsedTicks |> int |> string) + " FPS)"))
          titleUpdateTimer.Reset ()
        let timeFactor = (ticksToMilliseconds frameTimer.ElapsedTicks) / 20.  // The game runs at something in the neighborhood of 20 milliseconds/frame
        frameTimer.Reset ()
        
        runGame game gameRenderer timeFactor
    | None -> ()
  
#if MONOMAC
  override this.FinishedLaunching notification =
    let runGame = this.RunGame
#else
  let startGame () =
#endif
    Renderers.Game.Init ()
    use gameRenderer = new Renderers.Game()
    runGame (SuperHexagon.SuperHexagon.CreateDefault ()) gameRenderer 1.

module main =
  [<EntryPoint>]
  let main args =
    #if MONOMAC
    NSApplication.Init ()
    using (new NSAutoreleasePool()) (fun n -> 
      NSApplication.SharedApplication.Delegate <- new AppDelegate()
      NSApplication.Main args )
    #else
    GameLoop.startGame ()
    #endif
    0