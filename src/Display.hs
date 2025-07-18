module Display where

import Data.Word
import Data.Array.IArray
import Graphics.Gloss

import Types
import InterfaceTypes
import Config
import GameLogic

display :: InterfaceState -> Picture
display (GameLogic (Between Nothing _)) =
   translate (- winWidth' * 0.25) 0 .
   scale 0.4 0.4 . Text $ "Choose Level (0..6)"
display (GameLogic (Between (Just prevGame) _)) =
  pictures [ board prevGame
           , panel prevGame
           , instr
           , endPrompt prevGame
           ]
display (GameLogic (InGame state)) =
  pictures [ board state
           , panel state
           , instr
           ]

display (GameLogic (RowComplete _ prev _)) =
  pictures [ board prev
           , panel prev
           , instr
           ]

display (GameLogic (LevelUp _ state)) =
  pictures [ board state
           , panel state
           , instr
           ]

display (GameLogic (Paused state)) =
  pictures [ boardPaused state
           , panel state
           , instr
           ]

display (RowCompleteAnim rc old state fc) =
  pictures [ boardRCAnim rc (grid old) fc
           , panel state
           , instr
           ]

display (LevelUpAnim lvl state _) =
  pictures [ boardLevelUpAnim lvl
           , panel state
           , instr
           ]

rowCompleteAnimFrame :: Int
rowCompleteAnimFrame = 4

levelUpAnimFrame :: Int
levelUpAnimFrame = 10

-- In Game

board :: GameState -> Picture
board st | (t, _, _, minos) <- tet st =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         ( scale gridSize gridSize
            (pictures [ border, drawShadow (snd (shadow st))
                      , drawTetrad t minos])
         : drawGrid (grid st))


boardPaused :: GameState -> Picture
boardPaused st | (t, _, _, minos) <- tet st =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         [ scale gridSize gridSize border
         , translate 20 (brdHeight' * gridSize * 0.5) .
           scale 0.15 0.15 $
             text "Paused"
         , translate 10 (brdHeight' * gridSize * 0.5 - 20) .
           scale 0.1 0.1 $
             text "Press any key to continue"
         ]

boardRCAnim :: [Int] -> GridState -> Int -> Picture
boardRCAnim rc old fc =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         (scale gridSize gridSize border :
          drawGridAnim rc old fc)

boardLevelUpAnim :: Int -> Picture
boardLevelUpAnim lvl =
        translate (- brdWidth' * gridSize * 0.5)
                  (- brdHeight' * gridSize * 0.5) $
        pictures
         [ scale gridSize gridSize border
         , translate 20 (brdHeight' * gridSize * 0.5) .
           scale 0.15 0.15 $
             text ("Level " ++ show lvl)
         ]

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

coords :: [Pos]
coords = [(x,y) | x <- [0 .. brdWidth - 1], y <- [0 .. brdHeight]]

drawGridAnim :: [Int] -> GridState -> Int ->  [Picture]
drawGridAnim rc grid fc = map drawOneMino coords
   where drawOneMino pos@(x,y) = case grid ! pos of
            7 -> if x /= brdWidth - 1 then
                    dot (fromIntegral (x+1) * gridSize,
                         fromIntegral (y+1) * gridSize)
                   else Blank
            t -> scale gridSize gridSize
                  (if even fc && y `elem` rc then
                        drawMinoAlpha t pos 0.5
                   else drawMino t pos)

drawTetrad :: Tetrad -> Minos -> Picture
drawTetrad t minos =
   pictures $ map (drawMino t) (filter inBoard minos)
 where inBoard (x,y) = 0 <= x && x < brdWidth &&
                       0 <= y && y < brdHeight

drawTetrad' :: Tetrad -> Orientation -> Picture
drawTetrad' t ori = pictures $ map (drawMino t) (tetradSpec t ori)

drawShadow :: Minos -> Picture
drawShadow = pictures . map drawShadowMino

drawMino :: Tetrad -> Pos -> Picture
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

drawMinoAlpha :: Tetrad -> Pos -> Float -> Picture
drawMinoAlpha t (x,y) alpha =
  pictures [
      color (dim (tetColors ! t)) (square (x'+0.1, y'+0.1) 0.8)
    , color (dim (tetLighterColors ! t))
        (polygon [(x', y'+1), (x'+1, y'+1), (x'+0.9, y'+0.9), (x'+0.1, y'+0.9)])
    , color (dim (tetLightColors ! t))
        (polygon [(x', y'+1), (x'+0.1, y'+0.9), (x'+0.1, y'+0.1), (x', y')])
    , color (dim (tetDarkerColors ! t))
        (polygon [(x', y'), (x'+0.1, y'+0.1), (x'+0.9, y'+0.1), (x'+1, y')])
    , color (dim (tetDarkColors ! t))
        (polygon [(x'+1, y'), (x'+0.9, y'+0.1), (x'+0.9, y'+0.9), (x'+1, y'+1)])
    ]
 where (x',y') = (fromIntegral x, fromIntegral y)
       dim = withAlpha alpha

drawShadowMino :: Pos -> Picture
drawShadowMino (x,y) = color (withAlpha 0.5 (greyN 0.9)) (square (x',y') 1)
  where (x',y') = (fromIntegral x, fromIntegral y)

-- panel

panel :: GameState -> Picture
panel state =
   translate (brdWidth' * gridSize / 2 + 10) (gridSize * 10) .
   pictures $ [ rectBox (0,0) (gridSize * 4 + 60) (- 150)
              , next
              , levelInfo
              , rowCInfo
              , scoreInfo ]
 where next = translate (gridSize * 2 + 5) (- (gridSize * 2 + 10) )
                (scale gridSize gridSize (drawTetrad' (nextT state) TUp))
       textStartY = gridSize * 3 + 10
       levelInfo = translate 10 (- (textStartY + 10)) . scale 0.15 0.15
                    $ text ("Level: " ++ show (fst . lvl $ state))
       rowCInfo = translate 10 (- (textStartY + 35)) . scale 0.15 0.15
                    $ text ("Rows: " ++ show (rowsCleared state))
       scoreInfo = translate 10 (- (textStartY + 60)) . scale 0.15 0.15
                    $ text ("Score: " ++ show (score state))

-- game over prompt

endPrompt :: GameState -> Picture
endPrompt st = pictures $
  [ color (withAlpha 0.5 (greyN 0.9)) $ rectangleSolid 800 300
  , translate (- (winWidth' * 0.3)) 30 $
     linesOfText 0.3 40
              [ "Game Over"
               , "Score: " ++ show (score st)
               , "Choose Level (0..6) to Restart" ]
  ]

-- instructions panel

instr :: Picture
instr = pictures $
  [ translate (brdWidth' * gridSize / 2 + 10) 0 $
     linesOfText 0.15 25
      [ "arrows", "up", "space", "P", "R", "ESC"]
  , translate (brdWidth' * gridSize / 2 + 10 + 70) 0 $
     linesOfText 0.15 25
      [ "move", "rotate", "drop", "pause", "restart", "quit"]
  ]

-- picture elements

dot :: Point -> Picture
dot (x,y) = square (x,y) 1

square :: Point -> Float -> Picture
square (x,y) len =
  polygon [(x,y), (x+len,y), (x+len,y+len), (x, y+len)]

rectBox :: Point -> Float -> Float -> Picture
rectBox (x, y) width height =
  line [(x,y), (x+width ,y), (x+width,y+height), (x, y+height), (x,y)]

linesOfText :: Float -> Float -> [String] -> Picture
linesOfText size skip = pictures . zipWith ln [0..]
  where ln i xs = translate 0 (- i * skip) (scale size size (text xs))
