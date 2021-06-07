{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
import Text.Printf ( printf )

type Complex = (Double, Double)

-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
width, height, maxIter :: Int
width = 1080
height = 1920
maxIter = 255

coordY, coordX, scale :: Double
scale = 550000000
coordX = -1.940157358
coordY = 0


-----------------------------------------------------------
-- Criação de paletas de cor
-----------------------------------------------------------
filterPallete :: [(Double, Double, Double)] -> [(Pixel8, Pixel8, Pixel8)]
filterPallete = map (\(r,g,b) -> (round r, round g, round b))

pallete, laranja, azul, roxo, verde :: [(Pixel8, Pixel8, Pixel8)]
laranja = filterPallete (zip3 [0..254.0] [0,0.72..184] (replicate 254 0.0))

verde = filterPallete (zip3 [0,0.46..120] [0..255.0] (replicate 254 0.0))

azul = filterPallete (zip3 (replicate 254 0.0) (replicate 254 0.0) [0..255.0])

roxo = filterPallete (zip3 [0..254] (replicate 254 0.0) [0,0.59..150.0])

pallete = cycle (roxoInOut ++ laranjaInOut ++ verdeInOut ++ azulInOut)
  where laranjaInOut = laranja ++ reverse laranja
        verdeInOut = verde ++ reverse verde
        azulInOut = azul ++ reverse azul
        roxoInOut = roxo ++ reverse roxo


-----------------------------------------------------------
-- Cálculo do Mandelbrot
-----------------------------------------------------------
calcPoint :: Complex -> Complex -> Int -> Int
calcPoint (cx,cy) (zx,zy) iter
  | iter < maxIter && (zx + zy) < 4 = calcPoint (cx,cy) newZ (iter+1)
  | otherwise = iter
  where newZ = (zx*zx - zy*zy + cx , 2*zx*zy + cy)

colorFromIter :: Int -> Int -> PixelRGB8
colorFromIter iter hue = (\(r,g,b) -> PixelRGB8 r g b) n
  where n = pallete!!mod ((iter+hue)*6) (255*2*3)


-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
genStr :: [Int] -> [(String, Int)]
genStr = map (\x -> (printf "./etc/anim/%d.png" x, x))

--ffmpeg -framerate 24 -i %d.png -c:v libx264 -crf 25 -pix_fmt yuv420p output.mp4

main :: IO [()]
main = mapM doAnim (genStr [0..0])

doAnim :: (String, Int) -> IO ()
doAnim s =
  writePng (fst s) $ generateImage genPixel width height
    where genPixel x y = colorFromIter (calcPoint (xPos, yPos) (0,0) 0) (snd s)
            where xPos = (x'-w/2)/scale+coordX
                  yPos = (y'-h/2)/scale+coordY
                  (w,h) = (fromIntegral width, fromIntegral height)
                  (x', y') = (fromIntegral x, fromIntegral y)
