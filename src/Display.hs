module Display where

import Data.Word
import Data.Array.IArray
import Graphics.Gloss

import Types
import Config
import GameLogic

display :: State -> Picture
display (Between _ _) =
   translate (- winWidth' * 0.25) 0 .
   scale 0.4 0.4 . Text $ "Choose Level (0..7)"
display (InGame state) =
  pictures [ board state
           , translate (brdWidth' * gridSize * 0.5 + 40)
                       (6 * gridSize)
               (scale gridSize gridSize (drawTetrad' (nextT state) TUp))
           ]
display (CompeteAnim rc old fc state) =
  pictures [ boardAnim rc old fc
           , translate (brdWidth' * gridSize * 0.5 + 40)
                       (6 * gridSize)
               (scale gridSize gridSize (drawTetrad' (nextT state) TUp))
           ]


-- In Game

board :: GameState -> Picture
board st | (t, pos, ori) <- tet st =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         (scale gridSize gridSize border :
          scale gridSize gridSize (drawTetrad t pos ori) :
          drawGrid (grid st))

boardAnim :: [Int] -> GridState -> Int -> Picture
boardAnim rc old fc =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         (scale gridSize gridSize border :
          drawGridAnim rc old fc)

border :: Picture
border = Line [(0, brdHeight'), (0,0), (brdWidth', 0), (brdWidth', brdHeight')]

drawGrid :: GridState -> [Picture]
drawGrid grid = map drawOneMino coords
   where drawOneMino pos@(x,y) = case grid ! pos of
            7 -> if x /= brdWidth - 1 then
                    dot (fromIntegral (x+1) * gridSize,
                         fromIntegral (y+1) * gridSize)
                   else Blank
            t -> scale gridSize gridSize (drawMino t pos)

coords :: [(Int, Int)]
coords = [(x,y) | x <- [0 .. brdWidth - 1], y <- [0 .. brdHeight]]

drawGridAnim :: [Int] -> GridState -> Int ->  [Picture]
drawGridAnim rc grid fc = map drawOneMino
            (filter (\(_,y) -> odd fc || not (y `elem` rc)) coords)
   where drawOneMino pos@(x,y) = case grid ! pos of
            7 -> if x /= brdWidth - 1 then
                    dot (fromIntegral (x+1) * gridSize,
                         fromIntegral (y+1) * gridSize)
                   else Blank
            t -> scale gridSize gridSize (drawMino t pos)

{-
dots :: Picture
dots = pictures [dot (fromIntegral x * gridSize,
                      fromIntegral y * gridSize) |
         x <- [1 .. brdWidth - 1], y <- [1 .. brdHeight - 1]] -}

drawTetrad :: Tetrad -> (Int, Int) -> Orientation -> Picture
drawTetrad t pos ori =
   pictures $ map (drawMino t)
     (filter inBoard (minosOfATet t pos ori))
 where inBoard (x,y) = 0 <= x && x < brdWidth &&
                       0 <= y && y < brdHeight

drawTetrad' :: Tetrad -> Orientation -> Picture
drawTetrad' t ori = pictures $ map (drawMino t) (tetradSpec t ori)


drawMino :: Tetrad -> (Int,Int) -> Picture
-- simple "flat" style
-- drawTetrad t (x,y) =
--   color (tetColors ! t)
--   (square (fromIntegral x, fromIntegral y) 1)
drawMino t (x,y) =
  pictures [
      color (tetColors ! t) (square (x'+0.1, y'+0.1) 0.8)
    , color (tetLighterColors ! t)
        (polygon [(x', y'+1), (x'+1, y'+1), (x'+0.9, y'+0.9), (x'+0.1, y'+0.9)])
    , color (tetLightColors ! t)
        (polygon [(x', y'+1), (x'+0.1, y'+0.9), (x'+0.1, y'+0.1), (x', y')])
    , color (tetDarkerColors ! t)
        (polygon [(x', y'), (x'+0.1, y'+0.1), (x'+0.9, y'+0.1), (x'+1, y')])
    , color (tetDarkColors ! t)
        (polygon [(x'+1, y'), (x'+0.9, y'+0.1), (x'+0.9, y'+0.9), (x'+1, y'+1)])
    ]
 where (x',y') = (fromIntegral x, fromIntegral y)

-- picture elements

dot :: Point -> Picture
dot (x,y) = square (x,y) 1

square :: Point -> Float -> Picture
square (x,y) len =
  polygon [(x,y), (x+len,y), (x+len,y+len), (x, y+len)]
