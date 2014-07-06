namespace SuperHexagon

type IGameScreen =
  /// Return the next IGameScreen to use in the game. Don't use events, use keyboard states, because SDL key up and down events are unreliable.
  abstract Update: lastKeyboardState:byte[] -> keyboardState:byte[] -> timeFactor:float -> IGameScreen