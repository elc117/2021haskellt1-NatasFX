{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Text.Printf ( printf )
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Raster.Field
import Debug.Trace

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
    matrix :: [[Int]],
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
  matrix = [[ 0 | y <- [0..height]] | x <- [0..width]],
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

palleteColor :: [Color]
palleteColor = [makeColorI r g b 255 | (r,g,b) <- pallete]


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
colorFromIter hue iter = if iter == maxIter then makeColorI 0 0 0 0 else palleteColor!!mod (round $ hue + fromIntegral (iter+hueOffset)*6) maxi

-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
--ffmpeg -framerate 24 -i %d.png -c:v libx264 -crf 25 -pix_fmt yuv420p output.mp4

transPos :: Point -> (Int, Int)
transPos (x,y) = (round ((x+1) * w)+1, round ((y+1) * h)+1)
  where (w,h) = (fromIntegral width/2, fromIntegral height/2)

window :: Display
window = InWindow "Mandelbrot" (width, height) (10,10)

genPixel :: RenderState -> (Int,Int) -> Int
genPixel rs (x,y) = calcPoint (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size1+coordX
        yPos = (y'-h/2)/size1+coordY
        (w,h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)
        (size1, coordX, coordY) = (size rs, cX rs, cY rs)


recomputeMatrix :: RenderState -> RenderState
recomputeMatrix rs = rs {
  redraw = False,
  matrix = [[genPixel rs (x,y) | y <- [0..height]] | x <- [0..width]]--map (\l -> map (\iter -> genPixel rs (0,0)) l) $ matrix rs
}

distanceX :: (Int, Int) -> (Int, Int) -> Int
distanceX (x1,y1) (x2, y2) = (x2-x1)

distanceY :: (Int, Int) -> (Int, Int) -> Int
distanceY (x1,y1) (x2, y2) = (y2-y1)

render :: RenderState -> Point -> Color
render rs p = colorFromIter (hue rs) $ (\(x,y) -> matrix rs !! x !! y) (transPos p)--unsafeGet x y (matrix rs)) (transPos p)
{-# INLINE render #-}

eventHandler :: Event -> RenderState -> RenderState
eventHandler (EventKey (Char 'r') _ _ _) rs = traceShow ( show $ redraw rs ) (rs { redraw = True })


{- eventHandler (EventKey (MouseButton LeftButton ) Down _ (mX, mY)) rs = rs
 where rs = rs {
    redraw = True,
    dragging = True,
    dragStart = if fst (dragStart rs) == -1
      then transPos (mX, mY)
      else dragStart rs,
    cX = if dragging rs then cX rs + 0.5{-realToFrac (distanceX (dragStart rs) (transPos (mX, mY)))/size rs -} else cX rs,
    cY = if dragging rs then cY rs + 0.5{-realToFrac (distanceY (dragStart rs) (transPos (mX, mY)))/size rs -} else cY rs
  }

eventHandler (EventKey (MouseButton LeftButton ) Up _ _) rs = rs { dragging = False, dragStart = (-1,-1) }
-}
eventHandler (EventKey (MouseButton WheelUp) _ _ _) rs = rs { size = size rs + 100, redraw = True }
eventHandler (EventKey (SpecialKey KeyUp) _ _ _) rs = rs { size = size rs + 100, redraw = True }

eventHandler (EventKey (MouseButton WheelDown) _ _ _) rs = rs { size = if size rs <= 100 then size rs else size rs - 100, redraw = size rs > 100 }
eventHandler (EventKey (SpecialKey KeyDown) _ _ _) rs = rs { size = if size rs <= 100 then size rs else size rs - 100, redraw = size rs > 100 }
eventHandler _ rs = rs
{-# NOINLINE eventHandler #-}


update :: Float -> RenderState -> RenderState
update time rs
  | redraw rs = recomputeMatrix rs
  | otherwise = rs { hue = oldHue + 1 }
   where oldHue = hue rs
{-# NOINLINE update #-}

main :: IO ()
main =
  playField window (1,1) 60 initialState render eventHandler update

