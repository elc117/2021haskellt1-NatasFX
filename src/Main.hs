{-
    Programa em haskell para gerar Mandelbrot
    Trabalho 1 de Paradigmas de Programação
    Autoria: Natã Schmitt
-}

import Codec.Picture
type Complex = (Double, Double)

width, height :: Int
width = 960
height = 720

coordY, coordX :: Double
coordX = -0.5
coordY = 0



calculatePoint :: Complex -> Complex -> Int -> Int -> Int
calculatePoint (cx,cy) (zx,zy) iter maxIter = if iter < maxIter && (zx + zy) < 4 then calculatePoint (cx,cy) (zx*zx - zy*zy + cx , 2*zx*zy + cy) (iter+1) maxIter else iter

colorFromIter :: Int -> [Pixel8]
colorFromIter iter = [fromIntegral n,fromIntegral n,fromIntegral n]
    where n = if iter == 256 then 0 else mod (iter*3) 255

genPixel :: Double -> Double -> [Pixel8]
genPixel x y = colorFromIter (calculatePoint ((x-fromIntegral width/2)/size+coordX, (y-fromIntegral height/2)/size+coordY) (0,0) 0 255)
  where size = 300

toRGB :: Int -> Int -> PixelRGB8
toRGB x y = PixelRGB8 (head color) (fromIntegral (color!!1)) (fromIntegral (last color))
    where color = genPixel (fromIntegral x) (fromIntegral y)

main :: IO ()
main = writeBitmap "./mandelbrot.bmp" $ generateImage toRGB width height

