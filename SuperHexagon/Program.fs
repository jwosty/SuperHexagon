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
  
  member this.RunGame (game: SuperHexagon) (gameRenderer: Renderers.Game) lastKeyboardState lastTimeFactor =
    let runGame = this.RunGame
#else
module GameLoop =
  let titleUpdateTimer = new Stopwatch()
  let frameTimer = new Stopwatch()
  
  let rec runGame (game: SuperHexagon) (gameRenderer: Renderers.Game) lastKeyboardState lastTimeFactor =
#endif
    frameTimer.Start()
    if not titleUpdateTimer.IsRunning then titleUpdateTimer.Start ()
    let events = pollEvents ()
    let keyboardState = getKeyboardState ()
    
    match game.Update events lastKeyboardState keyboardState lastTimeFactor with
    | Some(game) ->
        gameRenderer.DrawFrame game
        
        // 20 milliseconds/frame cap
        let elapsed = ticksToMilliseconds frameTimer.ElapsedTicks
        if elapsed < 22. then Thread.Sleep (22. - elapsed |> floor |> int)
        
        if titleUpdateTimer.ElapsedMilliseconds >= 1000L then
          SDL.SDL_SetWindowTitle (gameRenderer.WindowHandle, ("Super Hexagon (" + (1000. / ticksToMilliseconds frameTimer.ElapsedTicks |> int |> string) + " FPS)"))
          titleUpdateTimer.Reset ()
        // Use 20 milliseconds/frame as the reference point. It doesn't really matter what this number is as long as it doesn't change and everything is calibrated to it.
        let timeFactor = (ticksToMilliseconds frameTimer.ElapsedTicks) / 20.
        frameTimer.Reset ()
        
        runGame game gameRenderer keyboardState timeFactor
    | None -> ()
  
#if MONOMAC
  override this.FinishedLaunching notification =
    let runGame = this.RunGame
#else
  let startGame () =
#endif
    Renderers.Game.Init ()
    use gameRenderer = new Renderers.Game()
    runGame (SuperHexagon.SuperHexagon.CreateDefault ()) gameRenderer (getKeyboardState ()) 1.

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