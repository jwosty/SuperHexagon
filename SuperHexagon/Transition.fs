namespace SuperHexagon

type Transition =
  { start: IGameScreen; finish: IGameScreen
    gameTimeDuration: float; gameTime: float }
  
  static member CreateDefault start finish totalDuration =
    { start = start; finish = finish
      gameTimeDuration = totalDuration; gameTime = 0. }
  
  interface IGameScreen with
    member this.Update keyboard timeFactor =
      if (this.gameTime + 1.) >= this.gameTimeDuration
      then this.finish
      else upcast { this with gameTime = this.gameTime + timeFactor }