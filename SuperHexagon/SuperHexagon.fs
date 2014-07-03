namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions

type SuperHexagon =
  | MidGame of Game | PostGame of PostGame
  
  static member CreateDefault () = MidGame(Game.CreateDefault ())
  
  member this.Update events keyboardState =
    if quitRequested events keyboardState then
      None
    else
      match this with
      | MidGame(game) -> Some(MidGame(game.Update keyboardState))
      | PostGame(postGame) -> Some(PostGame(postGame.Update keyboardState))