module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import System.Random (mkStdGen, initStdGen)

import Config
import qualified Display
import GameLogic
import Types


main :: IO ()
main = do randSeed <- initStdGen
          play windowDisplay white 15 (Between Nothing randSeed)
            Display.display input nextFrame

input :: Event -> State -> State
input (EventKey (SpecialKey KeyUp)    Down _ _) = flip nextState (KeyPressed KUp)
input (EventKey (SpecialKey KeyDown)  Down _ _) = flip nextState (KeyPressed KDown)
input (EventKey (SpecialKey KeyRight) Down _ _) = flip nextState (KeyPressed KRight)
input (EventKey (SpecialKey KeyLeft)  Down _ _) = flip nextState (KeyPressed KLeft)
input (EventKey (SpecialKey KeySpace) Down _ _) = flip nextState (KeyPressed KSpace)
input (EventKey (Char '0') Down _ _) = flip nextState (KeyPressed (KNum 0))
input (EventKey (Char '1') Down _ _) = flip nextState (KeyPressed (KNum 1))
input (EventKey (Char '2') Down _ _) = flip nextState (KeyPressed (KNum 2))
input (EventKey (Char '3') Down _ _) = flip nextState (KeyPressed (KNum 3))
input (EventKey (Char '4') Down _ _) = flip nextState (KeyPressed (KNum 4))
input (EventKey (Char '5') Down _ _) = flip nextState (KeyPressed (KNum 5))
input (EventKey (Char '6') Down _ _) = flip nextState (KeyPressed (KNum 6))
input _ = id

nextFrame :: Float -> State -> State
nextFrame _ = flip nextState TimeElapse

windowDisplay :: Display
windowDisplay = InWindow "Window" (winWidth, winHeight) (10, 10)
