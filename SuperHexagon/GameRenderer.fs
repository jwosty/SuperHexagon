namespace SuperHexagon.Renderers
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open SuperHexagon
open SuperHexagon.HelperFunctions
open SDL2
open System
open System.Runtime.InteropServices

type Game =
  val Handle: nativeint
  val GLContext: nativeint
  
  static member DefaultWindowFlags =
    SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL ||| SDL.SDL_WindowFlags.SDL_WINDOW_HIDDEN |||
    SDL.SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS ||| SDL.SDL_WindowFlags.SDL_WINDOW_MOUSE_FOCUS
  
  new(handle, glContext) =
    { Handle = handle
      GLContext = glContext }
  
  new() =
    SDL.SDL_SetMainReady ()
    SDL.SDL_Init SDL.SDL_INIT_VIDEO |> ignore
    
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
    
    let handle = SDL.SDL_CreateWindow ("SuperHexagon", SDL.SDL_WINDOWPOS_CENTERED, SDL.SDL_WINDOWPOS_CENTERED, 1024, 768, Game.DefaultWindowFlags)
    SDL.SDL_DisableScreenSaver ()
    
    // Create OpenGL context
    let glContext = SDL.SDL_GL_CreateContext handle
    GraphicsContext.CurrentContext <- glContext
    GL.LoadAll ()
    
    let mutable w, h = 0, 0
    SDL.SDL_GetWindowSize (handle, &w, &h)
    let w, h = float w, float h
    // Scale so that the viewport doesn't appear stretched, and invert the Y axis
    GL.Scale ((min w h) / w, (min w h) / -h, 1.)
    
    GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
    GL.Enable EnableCap.Blend
    // Enable multisampling (antialiasing)
    GL.Enable EnableCap.Multisample
    
    // Show the window
    SDL.SDL_ShowWindow handle
    
    new Game(handle, glContext)
  
  member this.GLMatrixDo render =
    GL.PushMatrix ()
    render ()
    GL.PopMatrix ()
  
  member this.GLDo beginMode render =
    GL.Begin beginMode
    render ()
    GL.End ()
  
  member private this.DrawObstacle playerSection (section, distance) =
    let x, y = Seq.nth section unitHexagonVertices
    let nextX, nextY = Seq.nth (section + 1 |> wrap 6) unitHexagonVertices
    if playerSection = section && distance >|< (0.12, 0.15)
    then GL.Color3 (1., 0., 0.)
    else GL.Color3 (0., 1., 0.)
    this.GLDo BeginMode.Triangles (fun () ->
      GL.Vertex2 (nextX * distance, nextY * distance)
      GL.Vertex2 (x * distance, y * distance)
      GL.Vertex2 (x * distance * 1.1, y * distance * 1.1)
      
      GL.Vertex2 (nextX * distance, nextY * distance)
      GL.Vertex2 (x * distance * 1.1, y * distance * 1.1)
      GL.Vertex2 (nextX * distance * 1.1, nextY * distance * 1.1))
  
  member private this.DrawCenterAndPlayer game =
    // Draw the player
    this.GLMatrixDo (fun () ->
      let factor = 0.1
      // Draw the inner hexagon
      GL.Color3 (0., 1., 0.)
      this.GLDo BeginMode.Triangles (fun () ->
        unitHexagonVertices
          |> Seq.fold (fun (lastX: float, lastY) (x, y) ->
              GL.Vertex2 (0, 0)
              GL.Vertex2 (lastX * factor, lastY * factor)
              GL.Vertex2 (x * factor, y * factor)
              x, y)
            (Seq.last unitHexagonVertices)
          |> ignore)
      // Draw the player triangle pointing in the appropriate direction
      GL.Color3 (0., 0.8, 0.)
      GL.Rotate (float game.playerAngle, 0., 0., 1.)
      GL.Translate (0., factor * -1.1, 0.)
      this.GLDo BeginMode.Triangles (fun () ->
        GL.Vertex2 (-0.0125,  0.  )
        GL.Vertex2 ( 0.    , -0.02)
        GL.Vertex2 ( 0.0125,  0.  )))
  
  member this.DrawMidGame game =
    // Rotation!
    this.GLMatrixDo (fun () ->
      //GL.Rotate (float game.ticks * -4., 0., 0., 1.)
      
      // Draw a point in the center
      this.GLDo BeginMode.Points (fun () -> GL.Vertex2 (0, 0))
      
      // Draw the background
      this.GLDo BeginMode.Triangles (fun () ->
        let far = 2.
        unitHexagonVertices
          |> Seq.fold (fun ((lastX: float, lastY), i) (x, y) ->
              if i % 2 = 0    // Alternating background colors
              then GL.Color3 (0., 0.25, 0.)   // light
              else GL.Color3 (0., 0.3, 0.)    // dark
              GL.Vertex2 (0, 0)
              GL.Vertex2 (lastX * far, lastY * far)
              GL.Vertex2 (x * far, y * far)
              (x, y), i + 1)
            (Seq.last unitHexagonVertices, 0)
          |> ignore)
      
      this.DrawCenterAndPlayer game
      
      List.iter (this.DrawObstacle (float game.playerAngle * (6. / 360.) |> wrap 6. |> int)) game.obstacles.obstacles)
  
  member this.DrawFrame game =
    GL.ClearColor (0.f, 0.f, 0.f, 1.f)
    GL.Clear ClearBufferMask.ColorBufferBit
    
    match game with
    | MidGame(game) -> this.DrawMidGame game
    | PostGame(game) -> ()
    
    SDL.SDL_GL_SwapWindow this.Handle

  member this.Window = SDL.SDL_GetWindowSurface this.Handle |> ptrToStructure<SDL.SDL_Surface>

  interface IDisposable with
    override this.Dispose () =
      SDL.SDL_GL_DeleteContext this.GLContext
      SDL.SDL_DestroyWindow this.Handle