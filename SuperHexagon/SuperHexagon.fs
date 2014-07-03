namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions

type SuperHexagon =
  { gameScreen: IGameScreen }
  
  static member CreateDefault () = { gameScreen = Game.CreateDefault () :> IGameScreen }
  
  member this.Update events keyboardState =
    if quitRequested events keyboardState
    then None
    else Some({this with gameScreen = this.gameScreen.Update keyboardState })