{-# LANGUAGE ExplicitNamespaces #-}
{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Text.Printf ( printf )
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)
import Graphics.Gloss.Raster.Field
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

import Data.Word

import Data.Array.CArray (CArray)

import Data.Array.IArray


import Data.Array.Accelerate
    ( type (:.)((:.)), Z(Z), fromList, Matrix, fill, constant)

import Data.Array.Accelerate.LLVM.Native


type Complex = (Double, Double)



-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
 
width, height, maxIter, hueOffset, mHeight, mWidth, scaleFactor :: Int
width = 800
height = 600
scaleFactor = 3
(mWidth, mHeight) = (width `div` scaleFactor, height `div` scaleFactor)

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
    matrix :: CArray (Int,Int) Int,
    redraw :: Bool,
    dragging :: Bool,
    dragStart :: (Int, Int)
  } deriving Show


initialState :: RenderState
initialState = State {
  size = 250,
  cX = -0.5,---1.940157358,
  cY = 0,
  hue = 1,
  matrix = array ((0,0),(mWidth,mHeight)) [((x,y), 0) | x <- [0..mWidth], y <- [0..mHeight]],
  redraw = True,
  dragging = False,
  dragStart = (-1, -1)
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

palleteColor :: [Color ]
palleteColor = [makeColorI r g b 255 | (r,g,b) <- pallete]


maxi :: Int
maxi = length laranja*2 + length verde*2 + length azul*2 + length roxo*2


-----------------------------------------------------------
-- Cálculo do Mandelbrot
-----------------------------------------------------------
calcPoint :: Complex -> Complex -> Int -> Int
calcPoint (cx,cy) (zx,zy) iter
  | iter < maxIter && (zx^2 + zy^2) < 4 = calcPoint (cx,cy) newZ (iter+1)
  | otherwise = iter
  where newZ = (zx*zx - zy*zy + cx , 2*zx*zy + cy)

colorFromIter :: Double -> Int -> Color
colorFromIter hue iter = if iter == maxIter then makeColorI 0 0 0 0 else palleteColor!!mod (round $ hue + fromIntegral (iter+hueOffset)*6) maxi

-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------

transPos :: Point -> (Int, Int)
transPos (x,y) = (round ((x+1) * w)+1, round ((y+1) * h)+1)
  where (w,h) = (fromIntegral mWidth/2, fromIntegral mHeight/2)

window :: Display
window = InWindow "Mandelbrot" (width, height) (10,10)

genPixel :: RenderState -> (Int,Int) -> Int
genPixel rs (x,y) = calcPoint (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size1+coordX
        yPos = (y'-h/2)/size1+coordY
        (w,h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)
        (size1, coordX, coordY) = (size rs, cX rs, cY rs)

cMatrix :: [Int] -> CArray (Int, Int) Int
cMatrix = listArray ((0, 0), (width,height))

recomputeMatrix :: RenderState -> RenderState
recomputeMatrix rs = rs {
  redraw = False,
  matrix = matrix rs // [((x,y), genPixel rs (x,y) ) | x <- [0..mWidth], y <- [0..mHeight]]
}

render :: RenderState -> Point -> Color
--render rs = bitmapOfArray (matrix rs) True
render rs p = colorFromIter (hue rs) $ matrix rs ! transPos p
{-# INLINE render #-}

eventHandler :: Event -> RenderState -> RenderState
eventHandler (EventKey (Char 'r') Down _ _) rs = traceShow ( show $ redraw rs ) (rs { redraw = True })

eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) rs = traceShow "click" rs { redraw = True, cX = cX rs - 0.1  }
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) rs = rs { redraw = True, cX = cX rs + 0.1  }


eventHandler (EventKey (MouseButton WheelUp) Down _ _) rs = rs { size = size rs + 100, redraw = True }
eventHandler (EventKey (MouseButton WheelDown) Down _ _) rs = rs { size = if size rs <= 100 then size rs else size rs - 100, redraw = size rs > 100 }


eventHandler (EventKey (SpecialKey KeyUp) Down _ _) rs = rs { size = size rs + 100, redraw = True }
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) rs = rs { size = if size rs <= 100 then size rs else size rs - 100, redraw = size rs > 100 }
eventHandler _ rs = rs
{-# NOINLINE eventHandler #-}


update :: Float -> RenderState -> RenderState
update time rs
  | redraw rs = recomputeMatrix rs
  | otherwise = rs { hue = hue rs + 1 }
{-# NOINLINE update #-}

main :: IO ()
main =
  playField window (scaleFactor,scaleFactor) 30 initialState render eventHandler update

