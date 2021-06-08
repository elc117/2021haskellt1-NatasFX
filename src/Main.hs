{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
import Text.Printf ( printf )
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix (Matrix, zero, unsafeGet, mapPos, mapCol, getElem, setElem, safeGet)
import Graphics.Gloss.Raster.Field

type Complex = (Double, Double)


-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
width, height, maxIter, hueOffset :: Int
width = 800
height = 600
maxIter = 255
hueOffset = 10--30

coordY, coordX, scaling :: Double
scaling = 400000000
coordX = -1.940157358
coordY = 0

data RenderState = State
  {
    size :: Double,
    cX :: Double,
    cY :: Double,
    hue :: Double,
    matrix :: Matrix Int,
    redraw :: Bool
  } deriving Show

initialState :: RenderState
initialState = State {
  size = 200,
  cX = -1.940157358,
  cY = 0,
  hue = 1,
  matrix = zero width height,
  redraw = True
}


-----------------------------------------------------------
-- Criação de paletas de cor
-----------------------------------------------------------
filterPallete :: [(Double, Double, Double)] -> [(Int, Int, Int)]
filterPallete = map (\(r,g,b) -> (round r, round g, round b))

pallete, laranja, azul, roxo, verde :: [(Int, Int, Int)]
laranja = filterPallete (zip3 [0..254.0] [0,0.72..184] (replicate 254 0.0))

verde = filterPallete (zip3 [0,0.46..120] [0..255.0] (replicate 254 0.0))

azul = filterPallete (zip3 (replicate 254 0.0) (replicate 254 0.0) [0..255.0])

roxo = filterPallete (zip3 [0..254] (replicate 254 0.0) [0,0.59..150.0])

pallete = azulInOut ++ laranjaInOut ++ verdeInOut ++ roxoInOut
  where laranjaInOut = laranja ++ reverse laranja
        verdeInOut = verde ++ reverse verde
        azulInOut = azul ++ reverse azul
        roxoInOut = roxo ++ reverse roxo

maxi :: Int
maxi = length laranja*2 + length verde*2 + length azul*2 + length roxo*2


-----------------------------------------------------------
-- Cálculo do Mandelbrot
-----------------------------------------------------------
calcPoint :: Complex -> Complex -> Int -> Int
calcPoint (cx,cy) (zx,zy) iter
  | iter < maxIter && (zx + zy) < 4 = calcPoint (cx,cy) newZ (iter+1)
  | otherwise = iter
  where newZ = (zx*zx - zy*zy + cx , 2*zx*zy + cy)

colorFromIter :: Double -> Int -> Color
--colorFromIter :: Double -> Int -> PixelRGB8 
colorFromIter hue iter = (\(r,g,b) -> makeColorI r g b 255) n
  where n = if iter == maxIter then (0,0,0) else pallete!!mod (round $ hue + fromIntegral (iter+hueOffset)*6) maxi


-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
--ffmpeg -framerate 24 -i %d.png -c:v libx264 -crf 25 -pix_fmt yuv420p output.mp4

window :: Display
window = InWindow "Mandelbrot" (width, height) (10,10)

genPixel :: RenderState -> (Int,Int) -> Int
genPixel rs (x,y) = calcPoint (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size rs+cX rs
        yPos = (y'-h/2)/size rs+cY rs
        (w,h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)


recomputeMatrix :: RenderState -> RenderState
recomputeMatrix rs = rs {
  redraw = False,
  matrix = mapPos (\x a -> genPixel rs x) $ matrix rs
}

render :: RenderState -> Point -> Color
render rs (x,y) = colorFromIter (hue rs) $ getElem (round ((x+1) * 400)+1) (round ((y+1) * 300)+1) (matrix rs)

eventHandler :: Event -> RenderState -> RenderState
eventHandler (EventKey (Char 'q') _ _ _) rs = rs
eventHandler _ rs = rs

update :: Float -> RenderState -> RenderState
update time rs
  | redraw rs = recomputeMatrix rs
  | otherwise = rs { hue = hue rs+1 }

main :: IO ()
main =
  playField window (1,1) 60 initialState render eventHandler update

