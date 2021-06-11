{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
import Text.Printf ( printf )
import Codec.FFmpeg.Juicy
import System.IO
import System.Exit
import Data.WAVE
import Math.FFT
import Data.Array.CArray
import qualified Data.Array.IArray as Arr
import Data.Array.Base
import Data.Complex
import Data.Maybe

type CComplex = (Double, Double)


-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
width, height, maxIter, hueOffset :: Int
width = 512
height = 512
maxIter = 150
hueOffset = 30

coordY, coordX, maxZoom :: Double
maxZoom = 1.6519276269095182e16
coordX = -1.940157358--0.36024044343761436323612524444954530848260780795858575048837581474019534605--
coordY = 0---0.64131306106480317486037501517930206657949495228230525955617754306444857417


-----------------------------------------------------------
-- Criação de paletas de cor
-----------------------------------------------------------
filterPallete :: [(Double, Double, Double)] -> [PixelRGB8]
filterPallete = map (\(r,g,b) -> PixelRGB8 (round r) (round g) (round b))

pallete, laranja, azul, roxo, verde :: [PixelRGB8]
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
calcPoint :: CComplex -> CComplex -> Int -> Int
calcPoint (cx,cy) (zx,zy) iter
  | iter < maxIter && (zx + zy) < 4 = calcPoint (cx,cy) newZ (iter+1)
  | otherwise = iter
  where newZ = (zx*zx - zy*zy + cx , 2*zx*zy + cy)

colorFromIter :: Int -> Int -> Int -> PixelRGB8
colorFromIter hue compression iter
  | iter == maxIter = PixelRGB8 0 0 0
  | otherwise = pallete!!mod ((iter+hue+hueOffset)*compression) maxi


-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
genPath :: Int -> FilePath
genPath = printf "./etc/anim/%d.png"


framerate :: Double
framerate = 30


carray :: [Double] -> CArray (Int, Int) Double
carray = Arr.listArray ((0, 0), (1024,0))

cMatrix :: [Int] -> CArray (Int, Int) Int
cMatrix = Arr.listArray ((0, 0), (width,height))

exitFalha :: String -> IO ()
exitFalha s = do
  a <- print s
  exitFailure


cleanComplex :: CArray (Int,Int) (Complex Double) -> Int
cleanComplex c = sum stripped `div` 10
  where stripped = take 10 cleaned
        cleaned = map (\c -> round (sqrt $ realPart c ^2 + imagPart c^2) `div` 100) $ elems c

main :: IO ()
main = do
  p <- getWAVEFile "./mandelbrot.wav" --Lendo nosso arquivo

  let header = waveHeader p

  let waveSmp = [fromIntegral (head l{- +last l -})/32768 | l <- waveSamples p] --todas nossas samples

  let samples = fromMaybe 0 $ waveFrames header

  s <- if samples == 0 then exitFalha "Falha na leitura de samples do arquivo." else putStrLn "\nFile OK."

  let duração = samples `div` waveFrameRate header

  let sampleRate = waveFrameRate header  --Numero de samples e.g 44100

  let samplesPerFrame = sampleRate `div` round framerate

  let offsetSamples = if samplesPerFrame > 1024 then 0 else 1024 - samplesPerFrame  --Não queremos menos que 1024 samples por frame, entao usaremos offset 

  let rangeFrames = [0..(duração * round framerate)]

  s <- putStrLn $ printf "SampleRate: %d\nduracao em segundos: %d\nNumero de frames: %d" sampleRate duração (duração * round framerate)
  s <- putStrLn $ printf "cada frame representará %d + %d offset samples" samplesPerFrame offsetSamples

  s <- putStrLn $ take 125 $ show $ carray waveSmp --impressao dos primeiros valores

  --s <- mapM_ (\x -> putStrLn $ printf "%d - %s" (x*samplesPerFrame `div` sampleRate) $ show $ cleanComplex $ dftRC $ carray $ drop ((samplesPerFrame+offsetSamples+1)*x) waveSmp) [0..(duração * round framerate)]


  let st = map (\x -> (x, cleanComplex (dftRC $ carray $ drop ((samplesPerFrame+offsetSamples)*x) waveSmp) `div` 100)) rangeFrames
  --let a = dftRC $ carray waveSmp
  --a <- print a
  s <- putStr "PRONTO??? [Y]"
  p <- hFlush stdout
  s <- getLine
  mapM_ (\x -> doAnim x samplesPerFrame sampleRate True) st



matrixUnica :: CArray (Int, Int) Int
matrixUnica = cMatrix [genIter x y $ -1 | y <- [0..height], x <- [0..width]]

genIter :: Int -> Int -> Int -> Int
genIter x y frame = calcPoint (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size+coordX
        yPos = (y'-h/2)/size+coordY
        (w, h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)
        size = 2** if frame == -1 then 17 else fromIntegral frame/framerate+7


readMatrix :: Int -> Int -> Int
readMatrix x y = matrixUnica ! (y,x)--matrixUnica !! x !! y

--ffmpeg -framerate 60 -i %d.png -c:v libx264 -crf 25 -pix_fmt yuv420p output.mp4
--ffmpeg -i output.mp4 -i ../../mandelbrot.wav -map 0:v -map 1:a -c:v copy -shortest output1.mp4

doAnim :: (Int, Int) -> Int -> Int -> Bool -> IO ()
doAnim s samplesPerFrame sampleRate static = do
  p <- putStr $ printf "Gerando %d level %d compression %d - %d" (fst s) hue compression (fst s*samplesPerFrame `div` sampleRate)
  p <- hFlush stdout
  p <- writePng path $ generateImage genPixel width height
  putStrLn " Done"
  where genPixel x y = colorFromIter hue compression $ if static then readMatrix x y else genIter x y frameN
        (path, frameN) = (genPath $ fst s, fromIntegral $ fst s)
        (h,c) = (snd s `div` 10, snd s `div` 35)
        hue = h
        compression = 6 + if c <= 1 then 0 else c

