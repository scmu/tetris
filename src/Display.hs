module Display where

import Data.Word
import Data.Array.IArray
import Graphics.Gloss

import Types
import InterfaceTypes
import Config
import GameLogic

calcDimensions :: (Int, Int) -> Dimensions
calcDimensions (ww, wh) = Dimens
   { canvasSize = (w, h)
   , gridSize   = gridSz

   , brdSize    = (gridSz * fromIntegral brdWidth, h)
   , brdShift   = (- w/2, - h/2)

   , panelSize     = (gridSz * 7, gridSz * 6)
   , panelShift    = (w/2 - gridSz, h/2)
   , panelTxtPad   = gridSz / 2
   , panelTxtScale = gridSz / glossFontHeight
   , panelTxtSkip  = gridSz

   , instrShift    = (w/2 - gridSz, 0 )
   , instrTxtScale = gridSz / glossFontHeight
   , instrTxtSkip  = gridSz

   , pauseTxtScale = gridSz / glossFontHeight
   , contTxtScale  = gridSz * 0.6 / glossFontHeight

   , lvlUpTxtScale = gridSz / glossFontHeight
   }
  where h = (fromIntegral wh - 2 * margin) `max` minCanvasHeight
        w = h * 2 / 3
        gridSz = h / fromIntegral brdHeight

glossFontHeight :: Float
glossFontHeight = 133 -- 200

---

display :: InterfaceState -> Picture
display is = displayAbs (dimens is) (presState is)

displayAbs :: Dimensions -> PresentationState -> Picture
displayAbs _ (GameLogic (Between Nothing _)) =
   translate (- winWidth' * 0.25) 0 .
   scale 0.4 0.4 . Text $ "Choose Level (0..6)"
displayAbs dimens (GameLogic (Between (Just prevGame) _)) =
  pictures [ board dimens prevGame
           , panel dimens prevGame
           , instr dimens
           , endPrompt prevGame
           ]
displayAbs dimens (GameLogic (InGame state)) =
  pictures [ board dimens state
           , panel dimens state
           , instr dimens
           ]

displayAbs dimens (GameLogic (RowComplete _ prev _)) =
  pictures [ board dimens prev
           , panel dimens prev
           , instr dimens
           ]

displayAbs dimens (GameLogic (LevelUp _ state)) =
  pictures [ board dimens state
           , panel dimens state
           , instr dimens
           ]

displayAbs dimens (GameLogic (Paused state)) =
  pictures [ boardPaused dimens state
           , panel dimens state
           , instr dimens
           ]

displayAbs dimens (RowCompleteAnim rc old state fc) =
  pictures [ boardRCAnim dimens rc (grid old) fc
           , panel dimens state
           , instr dimens
           ]

displayAbs dimens (LevelUpAnim lvl state _) =
  pictures [ boardLevelUpAnim dimens lvl
           , panel dimens state
           , instr dimens
           ]

rowCompleteAnimFrame :: Int
rowCompleteAnimFrame = 4

levelUpAnimFrame :: Int
levelUpAnimFrame = 10

resizeWindow :: (Int, Int) -> InterfaceState -> InterfaceState
resizeWindow (w, h) is = is { windowSize = (w, h)
                            , dimens = calcDimensions (w, h)}

-- In Game

board :: Dimensions -> GameState -> Picture
board dims st | (t, _, _, minos) <- tet st =
        translate shx shy $
        pictures
         ( scale gridSz gridSz
            (pictures [ border dims
                      , drawShadow (snd (shadow st))
                      , drawTetrad t minos ])
         : drawGrid dims (grid st))
    where (shx, shy) = brdShift dims
          gridSz = gridSize dims

boardPaused :: Dimensions -> GameState -> Picture
boardPaused dims st | (t, _, _, minos) <- tet st =
        translate shx shy $
        pictures
         [ scale gridSz gridSz (border dims)
         , translate 20 (w * 0.5) .
           scale sc1 sc1 $
             text "Paused"
         , translate 10 (w * 0.5 - gridSz) .
           scale sc2 sc2 $
             text "Press any key to continue"
         ]
    where (shx, shy) = brdShift dims
          gridSz = gridSize dims
          (w, h) = canvasSize dims
          sc1 = pauseTxtScale dims
          sc2 = contTxtScale dims

boardRCAnim :: Dimensions -> [Int] -> GridState -> Int -> Picture
boardRCAnim dims rc old fc =
        translate shx shy $
        pictures
         (scale gridSz gridSz (border dims) :
          drawGridAnim dims rc old fc)
    where (shx, shy) = brdShift dims
          gridSz = gridSize dims

boardLevelUpAnim :: Dimensions -> Int -> Picture
boardLevelUpAnim dims lvl =
        translate shx shy $
        pictures
         [ scale gridSz gridSz (border dims)
         , translate 20 (w * 0.5) .
           scale sc sc $
             text ("Level " ++ show lvl)
         ]
    where (shx, shy) = brdShift dims
          gridSz = gridSize dims
          (w, h) = canvasSize dims
          sc = lvlUpTxtScale dims


border :: Dimensions -> Picture
border dims = Line [(0, bh), (0,0), (bw, 0), (bw, bh)]
 where (bw, bh) = (brdWidth', brdHeight')

drawGrid :: Dimensions -> GridState -> [Picture]
drawGrid dims grid = map drawOneMino coords
   where drawOneMino pos@(x,y) = case grid ! pos of
            7 -> if x /= brdWidth - 1 then
                    dot (fromIntegral (x+1) * gridSz,
                         fromIntegral (y+1) * gridSz)
                   else Blank
            t -> scale gridSz gridSz (drawMino t pos)
         gridSz = gridSize dims

coords :: [Pos]
coords = [(x,y) | x <- [0 .. brdWidth - 1], y <- [0 .. brdHeight - 1]]

drawGridAnim :: Dimensions -> [Int] -> GridState -> Int ->  [Picture]
drawGridAnim dims rc grid fc = map drawOneMino coords
   where drawOneMino pos@(x,y) = case grid ! pos of
            7 -> if x /= brdWidth - 1 then
                    dot (fromIntegral (x+1) * gridSz,
                         fromIntegral (y+1) * gridSz)
                   else Blank
            t -> scale gridSz gridSz
                  (if even fc && y `elem` rc then
                        drawMinoAlpha t pos 0.5
                   else drawMino t pos)
         gridSz = gridSize dims

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

panel :: Dimensions -> GameState -> Picture
panel dims state =
   translate px py .
   pictures $ [ rectBox (0,0) pw (- ph)
              , next
              , info ]
 where next = translate (gridSz * 2 + 5) (- (gridSz * 2 + 10) )
                (scale gridSz gridSz (drawTetrad' (nextT state) TUp))
       textStartY = gridSz * 3 + 10
       info = translate (panelTxtPad dims) (- textStartY) $
              linesOfText (panelTxtScale dims) (panelTxtSkip dims)
                 [ "Level: " ++ show (fst . lvl $ state)
                 , "Rows: " ++ show (rowsCleared state)
                 , "Score: " ++ show (score state) ]
       gridSz = gridSize dims
       (px, py) = panelShift dims
       (pw, ph) = panelSize dims

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

instr :: Dimensions -> Picture
instr dims = pictures $
  [ translate sx sy $ linesOfText sc sk
      [ "arrows", "up", "space", "P", "R", "ESC"]
  , translate (sx + gridSz * 4) sy $ linesOfText sc sk
      [ "move", "rotate", "drop", "pause", "restart", "quit"]
  ]
  where gridSz = gridSize dims
        (sx, sy) = instrShift dims
        sc = instrTxtScale dims
        sk = instrTxtSkip dims

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
