module GameLogic where

import Data.Word
import Data.List (all)
import Data.Array.IArray
import Data.Array.Unboxed
import System.Random

import Config
import Types

nextState :: State -> GEvent -> State

nextState (Between _ seed) (KeyPressed (KNum lvl)) =
  InGame (initState (lvl `min` 6) seed)
nextState st@(Between _ _) _ = st

nextState (InGame state) TimeElapse =
  case frameCnt state of
    0 -> inGame (\st -> st { frameCnt = snd (lvl st) }) (fall state)
    n -> InGame (state { frameCnt = frameCnt state - 1 })
nextState (InGame state) (KeyPressed KLeft)  = moveLeft state
nextState (InGame state) (KeyPressed KRight) = moveRight state
nextState (InGame state) (KeyPressed KDown)  = moveDown state
nextState (InGame state) (KeyPressed KUp)    = simpRotate state
nextState (InGame state) (KeyPressed KSpace) = fastDrop state

nextState (CompeteAnim _ _ 0 state) TimeElapse = InGame state
nextState (CompeteAnim rc old n state) TimeElapse = CompeteAnim rc old (n-1) state
nextState st@(CompeteAnim _ _ _ _) _ = st

initState :: Int -> StdGen -> GameState
initState lvl seed =
     GState (lvl, ((6 - lvl) `max` 0))
            (t0, startingPos, TUp) t1
            initGrid

            ((6 - lvl) `max` 0)
            seed1
  where initGrid = genArray ((0,0), (brdWidth - 1, brdHeight + topBuffer - 1))
                     (const 7)
        (~(t0, seed0), ~(t1,seed1)) = (nextTet seed, nextTet seed0)

startingPos = (brdWidth `div` 2, brdHeight + 3)

type TetradSpec = Orientation -> [(Int,Int)]

specI :: TetradSpec
specI TUp    = [(-1,0), (0,0), (1,0), (2,0)]
specI TRight = [(0,-2), (0,-1), (0,0), (0,1)]
specI TDown  = [(-1,0), (0,0), (1,0), (2,0)]
specI TLeft  = [(0,-2), (0,-1), (0,0), (0,1)]

specL :: TetradSpec
specL TUp    = [(-1,0), (0,0), (1,0), (-1,1)]
specL TRight = [(0,-1), (0,0), (0,1), (1,1)]
specL TDown  = [(1,-1), (-1,0), (0,0), (1,0)]
specL TLeft  = [(-1,-1), (0,-1), (0,0), (0,1)]

specJ :: TetradSpec
specJ TUp    = [(-1,0), (0,0), (1,0), (1,1)]
specJ TRight = [(0,-1), (1,-1), (0,0), (0,1)]
specJ TDown  = [(-1,-1), (-1,0), (0,0), (1,0)]
specJ TLeft  = [(0,-1), (0,0), (-1,1), (0,1)]

specO :: TetradSpec
specO TUp    = [(0,0), (1,0), (0,1), (1,1)]
specO TRight = [(0,0), (1,0), (0,1), (1,1)]
specO TDown  = [(0,0), (1,0), (0,1), (1,1)]
specO TLeft  = [(0,0), (1,0), (0,1), (1,1)]

specS :: TetradSpec
specS TUp    = [(-1,0), (0,0), (0,1), (1,1)]
specS TRight = [(1,-1), (0,0), (1,0), (0,1)]
specS TDown  = [(-1,-1), (0,-1), (0,0), (1,0)]
specS TLeft  = [(0,-1), (-1,0), (0,0), (-1,1)]

specT :: TetradSpec
specT TUp    = [(-1,0), (0,0), (1,0), (0,1)]
specT TRight = [(0,-1), (0,0), (1,0), (0,1)]
specT TDown  = [(0,-1), (-1,0), (0,0), (1,0)]
specT TLeft  = [(0,-1), (-1,0), (0,0), (0,1)]

specZ :: TetradSpec
specZ TUp    = [(0,0), (1,0), (-1,1), (0,1)]
specZ TRight = [(0,-1), (0,0), (1,0), (1,1)]
specZ TDown  = [(0,-1), (1,-1), (-1,0), (0,0)]
specZ TLeft  = [(-1,-1), (-1,0), (0,0), (0,1)]

tetradSpec :: Tetrad -> TetradSpec
tetradSpec 0 = specI
tetradSpec 1 = specL
tetradSpec 2 = specJ
tetradSpec 3 = specO
tetradSpec 4 = specS
tetradSpec 5 = specT
tetradSpec 6 = specZ

  -- rowsToCheck for completion
rowsToCheck :: Array (Tetrad, Orientation) (Int, Int)
rowsToCheck = genArray ((0,TUp), (6,TLeft))
    (\(t, ori) -> let ys = map snd (tetradSpec t ori)
                  in (minimum ys, maximum ys))

minosOfATet t pos ori = zipWith plus (repeat pos) (tetradSpec t ori)
  where plus (x,y) (z,w) = (x+z, y+w)

nextTet :: StdGen -> (Tetrad, StdGen)
nextTet g = uniformR (0,6) g

-- movements

fall :: GameState -> State
fall st | noCollision (grid st) tetMinos =
           InGame $ st { tet = (t, pos', ori) }
        | {- collision and -} stuck y =
          Between (Just (grid st)) (randSeed st)  -- Game Over
        | otherwise = settleAndComplete st
  where (t, pos@(x,y), ori) = tet st
        pos' = (x,y-1)
        tetMinos = minosOfATet t pos' ori
        stuck y = y >= brdHeight

moveRight :: GameState -> State
moveRight st | noCollision (grid st) tetMinos =
                InGame $ st { tet = (t, pos', ori) }
             | otherwise = InGame st
 where (t, pos@(x,y), ori) = tet st
       pos' = (x+1,y)
       tetMinos = minosOfATet t pos' ori

moveLeft :: GameState -> State
moveLeft st | noCollision (grid st) tetMinos =
               InGame $ st { tet = (t, pos', ori) }
            | otherwise = InGame st
 where (t, pos@(x,y), ori) = tet st
       pos' = (x-1,y)
       tetMinos = minosOfATet t pos' ori

moveDown :: GameState -> State
moveDown st | noCollision (grid st) tetMinos =
               InGame $ st { tet = (t, pos', ori) }
            | otherwise = InGame st
  where (t, pos@(x,y), ori) = tet st
        pos' = (x,y-1)
        tetMinos = minosOfATet t pos' ori

simpRotate :: GameState -> State
simpRotate st | noCollision (grid st) tetMinos =
                InGame $ st { tet = (t, pos, ori') }
             | otherwise = InGame st
 where (t, pos@(x,y), ori) = tet st
       ori' = nextOri ori
       tetMinos = minosOfATet t pos ori'

nextOri TUp    = TRight
nextOri TRight = TDown
nextOri TDown  = TLeft
nextOri TLeft  = TUp

fastDrop :: GameState -> State
fastDrop st = settleAndComplete (st { tet = (t, findDropPos pos, ori)} )
 where (t, pos, ori) = tet st
       pos' = findDropPos pos
       findDropPos (x,y)
        | noCollision (grid st) (minosOfATet t (x,y-1) ori) = findDropPos (x,y-1)
        | otherwise = (x,y)

settleAndComplete :: GameState -> State
settleAndComplete st =
  let st' = st { tet = (nextT st, startingPos, TUp)
               , nextT = t'
               , grid = fuseIntoGridRmCompleted t tetMinos rc (grid st)
               , randSeed = g'}
  in case rc of [] -> InGame st'
                _  -> CompeteAnim rc (fuseIntoGrid (tet st) (grid st)) 4 st'
 where rc = rowsCompleted (tet st) tetMinos (grid st)
       (t, pos@(x,y), ori) = tet st
       tetMinos = minosOfATet t pos ori
       (t', g') = nextTet (randSeed st)

-- collision and completion checks

noCollision :: GridState -> [(Int, Int)] -> Bool
noCollision grid = all safe
   where safe (x,y) = 0 <= x && x < brdWidth && 0 <= y &&
                        (y >= brdHeight + topBuffer || grid ! (x,y) == 7)

rowsCompleted :: TetState -> [(Int, Int)] -> GridState -> [Int]
rowsCompleted (t, pos@(_,y), ori) minos grid =
   filter rowComplete rows
 where rows = let (i,j) = rowsToCheck ! (t, ori)
              in map (y+) [i..j]
       rowComplete y = all occupied [(x,y) | x <- [0 .. brdWidth - 1]]
       occupied (x,y) = grid ! (x,y) /= 7 || (x,y) `elem` minos

fuseIntoGrid :: TetState -> GridState -> GridState
fuseIntoGrid (t, pos, ori) grid =
  genArray ((0,0), (brdWidth - 1, brdHeight + topBuffer - 1)) gen
 where tetMinos = minosOfATet t pos ori
       gen p | p `elem` tetMinos = t
             | otherwise = grid ! p

fuseIntoGridRmCompleted :: Tetrad -> [(Int, Int)] -> [Int]
             -> GridState -> GridState
fuseIntoGridRmCompleted t minos compRows grid =
  genArray ((0,0), (brdWidth - 1, brdHeight + topBuffer - 1)) gen
 where gen p@(x,y)
        | y > brdHeight + topBuffer - 1 - length compRows = 7
        | (x,y') `elem` minos = t
        | otherwise = grid ! (x,y')
        where y' = yMap ! y

       yMap :: UArray Int Int  -- mapping new Y to old Y
       yMap = listArray (0, brdHeight + topBuffer - 1 - length compRows)
                (filter (not . (`elem` compRows))
                      [0 .. brdHeight + topBuffer - 1])
