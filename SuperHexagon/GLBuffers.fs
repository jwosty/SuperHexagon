namespace SuperHexagon
open System
open OpenTK.Graphics.OpenGL

type GLBuffers = 
  val indexBufferID: int;
  val vertexBufferID: int;
  val colorBufferID: int;
  
  new(indexBufferID, vertexBufferID, colorBufferID) = { indexBufferID = indexBufferID; vertexBufferID = vertexBufferID; colorBufferID = colorBufferID }
  
  static member private CreateBuffer bufferTarget (data: 'a[]) =
    let id = ref 0
    GL.GenBuffers (1, id)
    GL.BindBuffer (bufferTarget, !id)
    GL.BufferData (bufferTarget, nativeint <| data.Length*sizeof<'a>, data, BufferUsageHint.StaticDraw)
    !id
  
  new() =
    { indexBufferID = GLBuffers.CreateBuffer BufferTarget.ElementArrayBuffer [|0us..3us|]
      vertexBufferID = GLBuffers.CreateBuffer BufferTarget.ArrayBuffer [|-1.f;-1.f; 1.f;-1.f; 1.f;1.f; -1.f;1.f|]
      colorBufferID = GLBuffers.CreateBuffer BufferTarget.ArrayBuffer <| Array.create 16 1.f }