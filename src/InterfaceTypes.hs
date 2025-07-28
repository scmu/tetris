module InterfaceTypes where

import Types

data InterfaceState = IState
     { windowSize :: (Int, Int)       -- width, height
     , dimens     :: Dimensions
     , presState  :: PresentationState
     }

data Dimensions = Dimens
     { canvasSize :: (Float, Float)  -- width, height
     , gridSize   :: Float

     , brdSize    :: (Float, Float)
     , brdShift   :: (Float, Float)

     , panelSize     :: (Float, Float)
     , panelShift    :: (Float, Float)
     , panelTxtPad   :: Float
     , panelTxtScale :: Float
     , panelTxtSkip  :: Float

     , instrShift    :: (Float, Float)
     , instrTxtScale :: Float
     , instrTxtSkip  :: Float

     , pauseTxtScale :: Float
     , contTxtScale  :: Float

     , lvlUpTxtScale :: Float
     }

data PresentationState =   -- what we are currently showing the users
       GameLogic State
     | RowCompleteAnim [Int] GameState GameState Int
     | LevelUpAnim Int GameState Int

isGameLogic :: InterfaceState -> Maybe State
isGameLogic is | GameLogic state <- presState is = Just state
               | otherwise = Nothing

gameLogic :: (State -> State) -> InterfaceState -> InterfaceState
gameLogic f is | Just state <- isGameLogic is =
                     is { presState = GameLogic (f state)}
gameLogic f is = is
