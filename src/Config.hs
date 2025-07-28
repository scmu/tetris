module Config where

import Data.Word
import Data.Array.IArray
import Graphics.Gloss

import Types (Tetrad, tI, tJ, tL, tO, tS, tT, tZ)

winWidth, winHeight :: Int
winWidth  = 1000
winHeight = 600

winWidth', winHeight' :: Float
winWidth'  = fromIntegral winWidth
winHeight' = fromIntegral winHeight

margin :: Float
margin = 30

minCanvasHeight :: Float
minCanvasHeight = 300

---

brdWidth, brdHeight, topBuffer :: Int
brdWidth  = 10
brdHeight = 20
topBuffer = 4

brdWidth', brdHeight' :: Float
brdWidth'  = fromIntegral brdWidth
brdHeight' = fromIntegral brdHeight

--gridSize :: Float
--gridSize = 20

---

{- Use Word8 instead
data Tetrad = I | J | L | O | S | T | Z
  deriving (Eq, Show, Ord, Ix)

tetrads = [I, J, L, O, S, T, Z]
-}


tetMainColors :: [(Float, Float, Float)]
tetMainColors = map clamp
  [ (118, 181, 197)
  , ( 21,  76, 121)
  , (226, 135,  67)
  , (250, 227, 112)
  , (103, 166, 140)
  , (122, 101, 156)
  , (194,  66, 113)]
 where clamp = map3Tuple (\x -> fromIntegral x / 255)

map3Tuple :: (a -> b) -> (a,a,a) -> (b,b,b)
map3Tuple f (x,y,z) = (f x, f y, f z)

mkColor (r,g,b) = makeColor r g b 1

tetColors, tetLightColors, tetLighterColors
         , tetDarkColors,  tetDarkerColors  :: Array Tetrad Color
tetColors = array (tI,tZ) (zip [0..6] (map mkColor tetMainColors))
tetLightColors = array (tI,tZ) (zip [0..6]
    (map (mkColor . map3Tuple light) tetMainColors))
  where light x = x + (1-x) * 0.3
tetLighterColors = array (tI,tZ) (zip [0..6]
    (map (mkColor . map3Tuple lighter) tetMainColors))
  where lighter x = x + (1-x) * 0.6
tetDarkColors = array (tI,tZ) (zip [0..6]
    (map (mkColor . map3Tuple dark) tetMainColors))
  where dark = (0.7 *)
tetDarkerColors = array (tI,tZ) (zip [0..6]
    (map (mkColor . map3Tuple darker) tetMainColors))
  where darker = (0.5 *)
