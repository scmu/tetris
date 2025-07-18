module Types where

import Data.Word
import Data.Array.IArray
import Data.Array.Unboxed
import System.Random

data State = Between (Maybe GameState) -- state of previous game, if any
                     StdGen
           | InGame GameState
           | RowComplete [Int]      -- completed rows
                         GameState  -- previous game state
                         GameState  -- new game state
           | LevelUp Int        -- new level
                     GameState  -- new game state
           | Paused GameState

inGame :: (GameState -> GameState) -> State -> State
inGame f (InGame gst) = InGame (f gst)
inGame f st           = st

data GEvent = TimeElapse | KeyPressed Key
data Key = KUp | KDown | KLeft | KRight | KSpace | KNum Int
         | KP | KR | OtherKey

data GameState = GState
  { lvl         :: (Int, Int)  -- level, and maxFrameCnt = (6 - lvl) `max` 0
  , tet         :: TetState
  , nextT       :: Tetrad
  , grid        :: GridState
  , shadow      :: (Pos, Minos)

  , rowsCleared :: Int
  , score       :: Int

  , frameCnt    :: Int
  , randSeed    :: StdGen
  }

type Pos = (Int, Int)
type TetState = (Tetrad, Pos, Orientation, Minos)
type Minos = [Pos]
type GridState = UArray Pos Word8

data Orientation = TUp | TRight | TDown | TLeft
   deriving (Eq, Ord, Ix)

type Tetrad = Word8

tI, tJ, tL, tO, tS, tT, tZ :: Tetrad
tI = 0
tJ = 1
tL = 2
tO = 3
tS = 4
tT = 5
tZ = 6
