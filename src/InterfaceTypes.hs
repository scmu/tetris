module InterfaceTypes where

import Types

data InterfaceState =
       GameLogic State
     | RowCompleteAnim [Int] GridState GameState Int

gameLogic :: (State -> State) -> InterfaceState -> InterfaceState
gameLogic f (GameLogic state) = GameLogic (f state)
gameLogic f st = st
