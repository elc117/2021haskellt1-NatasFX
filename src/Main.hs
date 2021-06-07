{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
type Complex = (Double, Double)


width, height, maxIter :: Int
width = 1080
height = 1920
maxIter = 255

coordY, coordX, scale :: Double
scale = 600000000
coordX = -1.940157358
coordY = 0

filterPallete :: [(Double, Double, Double)] -> [(Pixel8, Pixel8, Pixel8)]
filterPallete = map (\(r,g,b) -> (round r, round g, round b)) 

pallete, palleteLaranja :: [(Pixel8, Pixel8, Pixel8)]
pallete = filterPallete (zip3 [0..254.0] [0,0.72..184] (replicate 254 0.0))

palleteLaranja = cycle (pallete ++ reverse pallete)


calcPoint :: Complex -> Complex -> Int  -> Int
calcPoint (cx,cy) (zx,zy) iter = if iter < maxIter && (zx + zy)/2 < 4 then calcPoint (cx,cy) (zx*zx - zy*zy + cx , 2*zx*zy + cy) (iter+1) else iter

colorFromIter :: Int -> PixelRGB8
colorFromIter iter = (\(r,g,b) -> PixelRGB8 r g b) n
    where n = palleteLaranja!!mod (iter*4) (length pallete*2)

main :: IO ()
main = 
  writeBitmap "./mandelbrot.bmp" $ generateImage genPixel width height
    where genPixel x y = colorFromIter (calcPoint (xPos, yPos) (0,0) 0)
            where xPos = (x'-w/2)/scale+coordX
                  yPos = (y'-h/2)/scale+coordY
                  (w,h) = (fromIntegral width, fromIntegral height)
                  (x', y') = (fromIntegral x, fromIntegral y)
