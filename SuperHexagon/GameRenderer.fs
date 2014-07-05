namespace SuperHexagon.Renderers
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open SuperHexagon
open SuperHexagon.HelperFunctions
open SDL2
open System
open System.Runtime.InteropServices

type Game =
  val WindowHandle: nativeint
  val GLContext: nativeint
  
  static member DefaultWindowFlags =
    SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL ||| SDL.SDL_WindowFlags.SDL_WINDOW_HIDDEN |||
    SDL.SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS ||| SDL.SDL_WindowFlags.SDL_WINDOW_MOUSE_FOCUS
  
  static member CenterHexagonRadius = 0.1
  static member PulseFactor = 0.01
  
  static member Init () =
    SDL.SDL_SetMainReady ()
    SDL.SDL_Init SDL.SDL_INIT_VIDEO |> ignore
    SDL_ttf.TTF_Init () |> ignore
  
  new(windowHandle, glContext) = { WindowHandle = windowHandle; GLContext = glContext }
  
  new() =
    
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_RED_SIZE, 8) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_GREEN_SIZE, 8) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_BLUE_SIZE, 8) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ALPHA_SIZE, 8) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore
    // Create a multisampling antialiasing buffer and use 4 samples per pixel
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_MULTISAMPLEBUFFERS, 1) |> ignore
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_MULTISAMPLESAMPLES, 4) |> ignore
    
#if DEBUG
    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG |> int) |> ignore
#endif
    
    
    let windowHandle = SDL.SDL_CreateWindow ("SuperHexagon", SDL.SDL_WINDOWPOS_CENTERED, SDL.SDL_WINDOWPOS_CENTERED, 1024, 768, Game.DefaultWindowFlags)
    SDL.SDL_DisableScreenSaver ()
    
    // Create OpenGL context
    let glContext = SDL.SDL_GL_CreateContext windowHandle
    GraphicsContext.CurrentContext <- glContext
    GL.LoadAll ()
    
    let mutable w, h = 0, 0
    SDL.SDL_GetWindowSize (windowHandle, &w, &h)
    let w, h = float w, float h
    // Scale so that the viewport doesn't appear stretched, and invert the Y axis
    GL.Scale ((min w h) / w, (min w h) / -h, 1.)
    
    GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
    GL.Enable EnableCap.Blend
    // Enable multisampling (antialiasing)
    GL.Enable EnableCap.Multisample
    GL.Enable EnableCap.Texture2D
    
    // Show the window
    SDL.SDL_ShowWindow windowHandle
    
    new Game(windowHandle, glContext)
  
  member this.GLMatrixDo render =
    GL.PushMatrix ()
    guarentee GL.PopMatrix render
  
  member this.GLDo beginMode render =
    GL.Begin beginMode
    guarentee GL.End render
  
  member this.DrawBackground (r,g,b) =
    this.GLDo BeginMode.Triangles (fun () ->
        let far = 2.
        let (lr, lg, lb), (dr, dg, db) = (r*0.3,g*0.3,b*0.3), (r*0.25,g*0.25,b*0.25)
        unitHexagonVertices
          |> Seq.fold (fun ((lastX: float, lastY), i) (x, y) ->
              if i % 2 = 0    // Alternating background colors
              then GL.Color3 (dr, dg, db)   // dark
              else GL.Color3 (lr, lg, lb)   // light
              GL.Vertex2 (0, 0)
              GL.Vertex2 (lastX * far, lastY * far)
              GL.Vertex2 (x * far, y * far)
              (x, y), i + 1)
            (Seq.last unitHexagonVertices, 0)
          |> ignore)
  
  member this.DrawBackgroundHexagon pulse (r,g,b) =
    // Draw a dark hexagon
    let rad = Game.CenterHexagonRadius + (pulse * Game.PulseFactor)
    GL.Color3 (r*0.15,g*0.15,b*0.15)
    this.GLDo BeginMode.Triangles (fun () ->
      unitHexagonVertices
        |> Seq.fold (fun (lastX: float, lastY) (x, y) ->
            GL.Vertex2 (0, 0)
            GL.Vertex2 (lastX * rad, lastY * rad)
            GL.Vertex2 (x * rad, y * rad)
            x, y)
          (Seq.last unitHexagonVertices)
        |> ignore)
    // Outline that hexagon
    GL.Color3 (r,g,b)
    this.GLDo BeginMode.LineStrip (fun () ->
      unitHexagonVertices |> Seq.iter (fun (x, y) -> GL.Vertex2 (x * rad, y * rad)))
  
  member this.DrawObstacle (r,g,b) pulse playerSection obstacle =
    let d = obstacle.distance + (pulse * Game.PulseFactor)
    let x, y = Seq.nth obstacle.section unitHexagonVertices
    let nextX, nextY = Seq.nth (obstacle.section + 1 |> wrap 6) unitHexagonVertices
    GL.Color3 (r*1.,g*1.,b*1.)
    this.GLDo BeginMode.Triangles (fun () ->
      GL.Vertex2 (nextX * d, nextY * d)
      GL.Vertex2 (x * d, y * d)
      GL.Vertex2 (x * d * 1.3, y * d * 1.3)
      
      GL.Vertex2 (nextX * d, nextY * d)
      GL.Vertex2 (x * d * 1.3, y * d * 1.3)
      GL.Vertex2 (nextX * d * 1.3, nextY * d * 1.3))
  
  member this.DrawPlayer pulse angle (r,g,b) =
    // Draw the player
    this.GLMatrixDo (fun () ->
      // Draw the player triangle pointing in the appropriate direction
      GL.Color3 (r*0.75,g*0.75,b*0.75)
      GL.Rotate (float angle, 0., 0., 1.)
      GL.Translate (0., -Game.CenterHexagonRadius - 0.011 - (pulse * Game.PulseFactor), 0.)
      this.GLDo BeginMode.Triangles (fun () ->
        GL.Vertex2 (-0.0125,  0.  )
        GL.Vertex2 ( 0.    , -0.02)
        GL.Vertex2 ( 0.0125,  0.  )))
  
  member this.DrawMidGame (game: SuperHexagon.Game) =
    this.GLMatrixDo (fun () ->
      GL.Rotate (game.rotation.screenAngle, 0., 0., 1.) // Rotation!
      let rgb = hsv2rgb (game.hue,1.,1.)
      let pulse = sin (game.gameTime / 16.) * 2.
      this.DrawBackground rgb
      List.iter (this.DrawObstacle rgb pulse <| int (angleToHexagonFace <| float game.playerAngle)) game.obstacles.items
      this.DrawBackgroundHexagon pulse rgb
      this.DrawPlayer pulse game.playerAngle rgb)
  
  member this.DrawPostGame (game: PostGame) =
    this.GLMatrixDo (fun () ->
      GL.Scale (5., 5., 1.)
      let rgb = hsv2rgb (game.hue,1.,1.)
      GL.Rotate (game.screenAngle, 0., 0., 1.)
      this.DrawBackground rgb
      this.DrawBackgroundHexagon 0. rgb)
  
  member this.DrawScreen (gameScreen: IGameScreen) =
    match gameScreen with
    | :? SuperHexagon.Game as game -> this.DrawMidGame game
    | :? PostGame as game -> this.DrawPostGame game
    | :? Transition as transition -> this.DrawTransition transition
    | _ -> failwith <| sprintf "Unimplemented \"%s\" screen" (gameScreen.GetType ()).Name
  
  member this.DrawTransition (transition: Transition) =
    match transition.start, transition.finish with
    | (:? SuperHexagon.Game as game), (:? PostGame as postGame) -> this.GLMatrixDo (fun () ->
        // Interpolate to get a smooth transition between the wrapped ending orientation and 0
        let endgameRotation = game.rotation.screenAngle |> wrap (360. / 3.)
        let p = float transition.gameTime / float transition.gameTimeDuration
        let scale = p * 4. + 1.
        GL.Scale (scale, scale, 1.)
        GL.Rotate ((lerp (endgameRotation, 0.) p), 0., 0., 1.)
        
        let (gr, gg, gb), (pr, pg, pb) = hsv2rgb (game.hue, 1., 1.), hsv2rgb (postGame.hue, 1., 1.)
        let rgb = lerp (gr,pr) p, lerp (gg,pg) p, lerp (gb, pb) p
        this.DrawBackground rgb
        this.DrawBackgroundHexagon 0. rgb)
    | (:? PostGame as postGame), (:? SuperHexagon.Game as game) -> this.GLMatrixDo (fun () ->
        let p = float transition.gameTime / float transition.gameTimeDuration
        let scale = 5. - 4. * p
        GL.Scale (scale, scale, 1.)
        GL.Rotate (p * 360. * (2./3.), 0., 0., 1.)
        
        let (gr, gg, gb), (pr, pg, pb) = hsv2rgb (postGame.hue, 1., 1.), hsv2rgb (game.hue, 1., 1.)
        let rgb = lerp (gr,pr) p, lerp (gg,pg) p, lerp (gb, pb) p
        this.DrawBackground rgb
        this.DrawBackgroundHexagon 0. rgb)
    | _ -> this.DrawScreen transition.finish  // If we don't know how to draw this particular transition, just draw the last screen instead of crashing
  
  member this.DrawFrame ({ gameScreen = gameScreen }) =
    GL.ClearColor (0.f, 0.f, 0.f, 1.f)        // Prepare for drawing
    GL.Clear ClearBufferMask.ColorBufferBit   // ^^
    this.DrawScreen gameScreen                // Draw
    SDL.SDL_GL_SwapWindow this.WindowHandle   // Make the result visible

  member this.Window = SDL.SDL_GetWindowSurface this.WindowHandle |> ptrToStructure<SDL.SDL_Surface>

  interface IDisposable with
    override this.Dispose () =
      SDL.SDL_GL_DeleteContext this.GLContext
      SDL.SDL_DestroyWindow this.WindowHandle