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
  
  member this.DrawBackground hue =
    this.GLDo BeginMode.Triangles (fun () ->
        let far = 2.
        let (lr, lg, lb), (dr, dg, db) = hsv2rgb (hue, 1., 0.3), hsv2rgb (hue, 1., 0.25)
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
  
  member this.DrawBackgroundHexagon hue =
    // Draw a dark hexagon
    let r,g,b = hsv2rgb (hue,1.,0.15)
    GL.Color3 (r,g,b)
    this.GLDo BeginMode.Triangles (fun () ->
      unitHexagonVertices
        |> Seq.fold (fun (lastX: float, lastY) (x, y) ->
            GL.Vertex2 (0, 0)
            GL.Vertex2 (lastX * Game.CenterHexagonRadius, lastY * Game.CenterHexagonRadius)
            GL.Vertex2 (x * Game.CenterHexagonRadius, y * Game.CenterHexagonRadius)
            x, y)
          (Seq.last unitHexagonVertices)
        |> ignore)
    // Outline that hexagon
    let r,g,b = hsv2rgb (hue,1.,1.)
    GL.Color3 (r,g,b)
    this.GLDo BeginMode.LineStrip (fun () ->
      unitHexagonVertices |> Seq.iter (fun (x, y) -> GL.Vertex2 (x * Game.CenterHexagonRadius, y * Game.CenterHexagonRadius)))
  
  member this.DrawObstacle hue playerSection obstacle =
    let x, y = Seq.nth obstacle.section unitHexagonVertices
    let nextX, nextY = Seq.nth (obstacle.section + 1 |> wrap 6) unitHexagonVertices
    let r,g,b = hsv2rgb (hue, 1., 1.)
    GL.Color3 (r,g,b)
    this.GLDo BeginMode.Triangles (fun () ->
      GL.Vertex2 (nextX * obstacle.distance, nextY * obstacle.distance)
      GL.Vertex2 (x * obstacle.distance, y * obstacle.distance)
      GL.Vertex2 (x * obstacle.distance * 1.3, y * obstacle.distance * 1.3)
      
      GL.Vertex2 (nextX * obstacle.distance, nextY * obstacle.distance)
      GL.Vertex2 (x * obstacle.distance * 1.3, y * obstacle.distance * 1.3)
      GL.Vertex2 (nextX * obstacle.distance * 1.3, nextY * obstacle.distance * 1.3))
  
  member this.DrawPlayer hue angle =
    // Draw the player
    this.GLMatrixDo (fun () ->
      // Draw the player triangle pointing in the appropriate direction
      let r,g,b = hsv2rgb (hue,0.5,0.8)
      GL.Color3 (r,g,b)
      GL.Rotate (float angle, 0., 0., 1.)
      GL.Translate (0., Game.CenterHexagonRadius * -1.1, 0.)
      this.GLDo BeginMode.Triangles (fun () ->
        GL.Vertex2 (-0.0125,  0.  )
        GL.Vertex2 ( 0.    , -0.02)
        GL.Vertex2 ( 0.0125,  0.  )))
  
  member this.DrawMidGame (game: SuperHexagon.Game) =
    this.GLMatrixDo (fun () ->
      GL.Rotate (game.screenAngle, 0., 0., 1.) // Rotation!
      this.DrawBackground game.hue
      List.iter (this.DrawObstacle game.hue <| int (angleToHexagonFace <| float game.playerAngle)) game.obstacles
      this.DrawBackgroundHexagon game.hue
      this.DrawPlayer game.hue game.playerAngle)
  
  member this.DrawPostGame (game: PostGame) =
    this.GLMatrixDo (fun () ->
      GL.Scale (5., 5., 1.)
      GL.Rotate (game.screenAngle, 0., 0., 1.)
      this.DrawBackground game.hue
      this.DrawBackgroundHexagon game.hue)
      (*
      GL.BindTexture (TextureTarget.Texture2D, this.GameOverTextureID)
      this.GLDo BeginMode.Quads (fun () ->
        GL.Vertex2 (-0.5, -0.5)
        GL.Vertex2 ( 0.5, -0.5)
        GL.Vertex2 ( 0.5,  0.5)
        GL.Vertex2 (-0.5,  0.5))) *)
    //SDL.SDL_BlitSurface (this.GameOverSurfacePtr, IntPtr.Zero, this.Handle, IntPtr.Zero) |> ignore
    ()
  
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
        let endgameRotation = game.screenAngle |> wrap (360. / 3.)
        let p = float transition.progress / float transition.finishTicks
        let scale = p * 4. + 1.
        GL.Scale (scale, scale, 1.)
        GL.Rotate ((lerp (endgameRotation, 0.) p), 0., 0., 1.)
        let hue = lerp (game.hue, postGame.hue) p
        this.DrawBackground hue
        this.DrawBackgroundHexagon hue)
    | (:? PostGame as postGame), (:? SuperHexagon.Game as game) -> this.GLMatrixDo (fun () ->
        let p = float transition.progress / float transition.finishTicks
        let scale = 5. - 4. * p
        GL.Scale (scale, scale, 1.)
        GL.Rotate (p * 360. * (2./3.), 0., 0., 1.)
        let hue = lerp (postGame.hue, game.hue) p
        this.DrawBackground hue
        this.DrawBackgroundHexagon hue)
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