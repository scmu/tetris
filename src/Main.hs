module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import System.Random (mkStdGen, initStdGen)

import Config
import qualified Display
import GameLogic
import Types
import InterfaceTypes

main :: IO ()
main = do randSeed <- initStdGen
          play windowDisplay white 15 (GameLogic (Between Nothing randSeed))
            Display.display input nextFrame

input :: Event -> InterfaceState -> InterfaceState
input (EventKey (SpecialKey KeyUp)    Down _ _) =
  gameLogic (flip nextState (KeyPressed KUp))
input (EventKey (SpecialKey KeyDown)  Down _ _) =
  gameLogic (flip nextState (KeyPressed KDown))
input (EventKey (SpecialKey KeyRight) Down _ _) =
  gameLogic (flip nextState (KeyPressed KRight))
input (EventKey (SpecialKey KeyLeft)  Down _ _) =
  gameLogic (flip nextState (KeyPressed KLeft))
input (EventKey (SpecialKey KeySpace) Down _ _) =
  gameLogic (flip nextState (KeyPressed KSpace))
input (EventKey (Char 'P') Down _ _) =
  gameLogic (flip nextState (KeyPressed KP))
input (EventKey (Char 'p') Down _ _) =
  gameLogic (flip nextState (KeyPressed KP))
input (EventKey (Char 'R') Down _ _) =
  gameLogic (flip nextState (KeyPressed KR))
input (EventKey (Char 'r') Down _ _) =
  gameLogic (flip nextState (KeyPressed KR))
input (EventKey (Char '0') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 0)))
input (EventKey (Char '1') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 1)))
input (EventKey (Char '2') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 2)))
input (EventKey (Char '3') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 3)))
input (EventKey (Char '4') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 4)))
input (EventKey (Char '5') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 5)))
input (EventKey (Char '6') Down _ _) =
  gameLogic (flip nextState (KeyPressed (KNum 6)))
input (EventKey (Char _) Down _ _) =
  gameLogic (flip nextState (KeyPressed OtherKey))
input _ = id

nextFrame :: Float -> InterfaceState -> InterfaceState
nextFrame _ (GameLogic (RowComplete rc prev state)) =
    RowCompleteAnim rc prev state Display.rowCompleteAnimFrame
nextFrame _ (RowCompleteAnim rc prev state 0) =
    GameLogic (nextState (RowComplete rc prev state) TimeElapse)
nextFrame _ (RowCompleteAnim rc prev state n) =
    RowCompleteAnim rc prev state (n-1)

nextFrame _ (GameLogic (LevelUp lvl state)) =
    LevelUpAnim lvl state Display.levelUpAnimFrame
nextFrame _ (LevelUpAnim lvl state 0) =
    GameLogic (nextState (LevelUp lvl state) TimeElapse)
nextFrame _ (LevelUpAnim lvl state n) =
    LevelUpAnim lvl state (n-1)

nextFrame _ (GameLogic state) = GameLogic (nextState state TimeElapse)


windowDisplay :: Display
windowDisplay = InWindow "Window" (winWidth, winHeight) (10, 10)
