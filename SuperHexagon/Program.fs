namespace SuperHexagon
open MonoMac.AppKit
open MonoMac.Foundation
open SDL2
open SuperHexagon.HelperFunctions
open System.Diagnostics
open System.Runtime.InteropServices
open System.Threading

type AppDelegate() = 
  inherit NSApplicationDelegate()
  let titleUpdateTimer = new Stopwatch()
  let frameTimer = new Stopwatch()
  
  member this.RunGame (game: SuperHexagon) (gameRenderer: Renderers.Game) =
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
          SDL.SDL_SetWindowTitle (gameRenderer.Handle, ("Super Hexagon (" + (1000. / ticksToMilliseconds frameTimer.ElapsedTicks |> int |> string) + " FPS)"))
          titleUpdateTimer.Reset ()
        
        frameTimer.Reset ()
        
        this.RunGame game gameRenderer
    | None -> ()
  
  override this.FinishedLaunching notification =
    use gameRenderer = new Renderers.Game()
    this.RunGame (SuperHexagon.SuperHexagon.CreateDefault ()) gameRenderer
    ()
  
  override this.ApplicationShouldTerminateAfterLastWindowClosed(sender) =
    true
 
module main =
  [<EntryPoint>]
  let main args =
    NSApplication.Init ()
    using (new NSAutoreleasePool()) (fun n -> 
      NSApplication.SharedApplication.Delegate <- new AppDelegate()
      NSApplication.Main args )
    0