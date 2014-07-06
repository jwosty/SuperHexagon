namespace SuperHexagon
open SDL2
open SuperHexagon.HelperFunctions

type SuperHexagon =
  { gameScreen: IGameScreen }
  
  static member CreateDefault () = { gameScreen = MainMenu.CreateDefault () :> IGameScreen }
  
  member this.Update events keyboardState timeFactor =
    if quitRequested events keyboardState
    then None
    else Some({this with gameScreen = this.gameScreen.Update keyboardState timeFactor })