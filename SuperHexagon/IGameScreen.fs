namespace SuperHexagon

type IGameScreen =
  abstract Update: keyboardState:byte[] -> timeFactor:float -> IGameScreen