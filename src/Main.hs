{-
  Programa em haskell para gerar Mandelbrot
  Trabalho 1 de Paradigmas de Programação
  Autoria: Natã Schmitt
-}

import Codec.Picture
import Text.Printf ( printf )
import Codec.FFmpeg.Juicy ( imageWriter )
import Codec.FFmpeg.Encode
import System.IO
import System.Exit ( exitFailure, exitSuccess )
import Data.WAVE
import Math.FFT ( dftRC )
import Data.Array.CArray
import qualified Data.Array.IArray
import Data.Complex
import Data.Maybe
import Data.Char


type CComplex = (Double, Double)


-----------------------------------------------------------
-- Variáveis pré-definidas
-----------------------------------------------------------
musicPath, videoPath, imagesPath :: FilePath
width, height, maxIter, hueOffset, sensitivity :: Int
coordY, coordX, maxZoom, framerate, fixedZoom :: Double

musicPath = "./mandeloso.wav"
videoPath = "./mandelbrot.mp4"
imagesPath = "./anim/%d.png"

width = 512           -- largura
height = 512         -- altura
maxIter = 400        -- numero maximo de iterações
hueOffset = 100         -- caso queira que a cor inicial seja diferente 

maxZoom = 1.0519276269095182e16               --Zoom maximo suportado
-- -1.940157358
coordX =  0.36024044343761436323612524444954530848260780795858575048837581474019534605  -- Coordenadas X
coordY = -0.64131306106480317486037501517930206657949495228230525955617754306444857417  -- Coordenadas Y
framerate = 30        -- Framerate do vídeo

sensitivity = 15      -- Sensibilidade ao grave maior = menos sensível
fixedZoom = 13--27.8  -- Zoom caso estático for True


estático :: Bool
estático = True       -- Quando True ele não fará zoom e se manterá estático em fixedZoom




-----------------------------------------------------------
-- Funções diversas
-----------------------------------------------------------
fst3, snd3, thr :: (a, a, a) -> a
fst3 (x, _, _) = x
snd3 (_, x, _) = x
thr (_, _, x)  = x

genPath :: Int -> FilePath
genPath = printf imagesPath

exitFalha :: String -> IO ()
exitFalha s = do
  a <- putStrLn s
  exitFailure

toFloat :: Int -> Double
toFloat i = fromIntegral i :: Double

status :: Int -> Double -> IO ()
status total n = do
  putStr (printf "\r%.f%%\t- %d/%d Frames" (n/toFloat total*100) (round n::Int) total)


-----------------------------------------------------------
-- Criação de paletas de cor
-----------------------------------------------------------
filterPallete :: [(Double, Double, Double)] -> [(Pixel8,Pixel8,Pixel8)]
filterPallete = map (\(r,g,b) -> (round r, round g, round b))

laranja, azul, roxo, verde :: [(Pixel8, Pixel8, Pixel8)]
laranja = filterPallete $ zip3 [0..254.0] [0,0.72..184] (replicate 254 0.0)

verde = filterPallete $ zip3 [0,0.46..120] [0..255.0] (replicate 254 0.0)

azul = filterPallete $ zip3 (replicate 254 0.0) (replicate 254 0.0) [0..255.0]

roxo = filterPallete $ zip3 [0..254] (replicate 254 0.0) [0,0.59..150.0]

pallete :: [(Pixel8,Pixel8,Pixel8)]
pallete = azulInOut ++ laranjaInOut ++ verdeInOut ++ roxoInOut
  where laranjaInOut = laranja ++ reverse laranja
        verdeInOut = verde ++ reverse verde
        azulInOut  = azul ++ reverse azul
        roxoInOut  = roxo ++ reverse roxo

palleteR, palleteG, palleteB :: CArray Int Pixel8
(palleteR, palleteG, palleteB) = ( palVec [fst3 x | x <- pallete],
                                   palVec [snd3 x | x <- pallete],
                                   palVec [thr x | x <- pallete]
                                 )

maxi :: Int
maxi = length laranja*2 + length verde*2 + length azul*2 + length roxo*2


-----------------------------------------------------------
-- Cálculo do Mandelbrot
-----------------------------------------------------------
calcPoint :: CComplex -> CComplex -> Int -> (Int, Double)
calcPoint (cx,cy) (zx,zy) iter
  | iter < maxIter && sqrt(zx^2 + zy^2) < 4 = calcPoint (cx,cy) newZ (iter+1)
  | otherwise = (iter, magnitude (zx:+zy) )
  where newZ = (zx^2 - zy^2 + cx , 2*zx*zy + cy)

colorFromIter :: Int -> (Int, Double) -> PixelRGB8
colorFromIter hue iterPoint
  | iter == maxIter = PixelRGB8 0 0 0
  | otherwise = PixelRGB8 (palleteR!i) (palleteG!i) (palleteB!i)
  where (iter,mag) = iterPoint
        i = truncate $ color * toFloat maxi
        color = toFloat ix / toFloat points -- Entre 0 e 1, representa um ciclo de cores
        ix      = truncate (sqrt (toFloat iter + 1 - logBase 2 (logBase 2 mag))*256 + toFloat hue*5 + toFloat hueOffset ) `mod` points
        points  = 2048

genIter :: Int -> Int -> Int -> Int -> (Int, Double)
genIter  x y frame db = calcPoint (xPos, yPos) (0,0) 0
  where xPos = (x'-w/2)/size+coordX
        yPos = (y'-h/2)/size+coordY
        (w, h) = (fromIntegral width, fromIntegral height)
        (x', y') = (fromIntegral x, fromIntegral y)
        size = 2** if frame == -1 then fixedZoom else exponent
        exponent = if exp > maxZoom then maxZoom else exp
        exp = fromIntegral (frame + db `div` 180)/(framerate*10)+7


-----------------------------------------------------------
-- Funções de matrizes/vetores
-----------------------------------------------------------
carray :: [Double] -> CArray (Int, Int) Double
carray = listArray ((0, 0), (1024,0))

cMatrix :: [Int] -> CArray (Int, Int) Int
cMatrix = listArray ((0, 0), (width,height))

cMatrixDouble :: [Double] -> CArray (Int, Int) Double
cMatrixDouble = listArray ((0, 0), (width,height))

matrixIter :: CArray (Int, Int) Int
matrixIter = cMatrix [fst $ genIter x y (-1) 0 | x <- [0..width], y <- [0..height]]

matrixMag :: CArray (Int, Int) Double
matrixMag = cMatrixDouble [snd $ genIter x y (-1) 0 | x <- [0..width], y <- [0..height]]

readIter :: Int -> Int -> Int
readIter x y = matrixIter ! (x,y)

readMag :: Int -> Int -> Double
readMag x y = matrixMag ! (x,y)

palVec :: [Pixel8] -> CArray Int Pixel8
palVec = listArray (0,maxi)


-----------------------------------------------------------
-- Funções FFT
-----------------------------------------------------------
cleanComplex :: CArray (Int,Int) (Complex Double) -> Int
cleanComplex c = sum stripped `div` 10
  where stripped = take 7 cleaned
        cleaned = map (\comp -> round (magnitude comp) `div` 100)  $ elems c

getAnimationTimings :: [Double] -> [Int] -> Int -> Int -> [((Int, Int), Int)]
getAnimationTimings samples rangeFrames spf dur = map (\(sz, db) -> ((sz, db), foldl1 (\n x -> n + x `div` 15) $ take sz [snd t | t <- newSt])) newSt
  where newSt = map (\sz -> (sz, foldl1 (\n x -> n - (n-x) `div` 4) $ take sz st)) [2..dur * round framerate]
        st = map (\x -> cleanComplex $ dftRC $ carray $ drop (spf*x+1) samples) rangeFrames


-----------------------------------------------------------
-- Criação da imagem
-----------------------------------------------------------
--ffmpeg -framerate 30 -i %d.png -c:v libx264 -crf 10 -pix_fmt yuv420p output.mp4 && ffmpeg -i output.mp4 -i ../mandelbrot.wav -map 0:v -map 1:a -c:v copy -shortest output1.mp4
--ffmpeg -i output.mp4 -i ../../mandelbrot.wav -map 0:v -map 1:a -c:v copy -shortest output1.mp4

doAnim :: ((Int, Int),Int) -> Bool -> Maybe (Image PixelRGB8)
doAnim info static = Just $ generateImage genPixel width height
  --p <- writePng path $ generateImage genPixel width height
  where genPixel x y = colorFromIter hue $ if static then (readIter x y, readMag x y) else genIter x y frameN hueDB
        h = hueDB `div` (10*sensitivity)
        hue = mod (db `div` (30*sensitivity) + h + frameN `div` 60) maxi
        (path, frameN, db) = (genPath $ fst s, fromIntegral $ fst s, snd s)
        (s, hueDB) = info

doAnimSave :: ((Int, Int),Int) -> FilePath -> Bool -> IO ()
doAnimSave info path static = do

  let image = fromMaybe (generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral x) (fromIntegral x)) 1 1 ) $ doAnim info static

  if imageWidth image == 0 then putStr "" else writePng path image


writeVideo :: [((Int, Int), Int)] -> Int -> Int -> Int ->  IO ()
writeVideo dbList spf sr total = do
  let listOfImage = map (`doAnim` estático) dbList ++ [Nothing]

  save <- imageWriter (EncodingParams (fromIntegral width) (fromIntegral height) (round framerate) Nothing Nothing "medium" Nothing) videoPath

  mapM_ (\(n,z) -> save z >> status total n) $ zip [1..] listOfImage


main :: IO ()
main = do
  _ <- hSetBuffering stdout NoBuffering

  p <- getWAVEFile musicPath --Lendo nosso arquivo

  let header = waveHeader p

  -- Esse número 32768 é quando estamos lendo word8 signed como unsigned
  -- então o que deveria ser 0 signed se torna 32768 unsigned (10000000 = -32768 signed but 10000000 unsiged = 32768)
  -- que aparentemente a lib WAVE lê incorretamente (?)
  -- o FFT não se importa mt, math is beautiful
  let waveSmpAll = [fromIntegral (sum l) /32768/ toFloat (waveNumChannels header)| l <- waveSamples p] --todas nossas samples

  let sampleRate = waveFrameRate header  --Numero de samples e.g 44100

  let waveSmp = drop (sampleRate `div` 10) waveSmpAll -- Isso irá colocar nosso vídeo 100ms adiantado, melhor sincronismo

  let samples = fromMaybe 0 $ waveFrames header     -- Total de samples da nossa música

  _ <- if samples == 0 then exitFalha "Falha na leitura de samples do arquivo." else putStrLn "\nFile OK."

  let duração = samples `div` waveFrameRate header

  let samplesPerFrame = sampleRate `div` round framerate

  let offsetSamples = if samplesPerFrame > 1024 then 0 else 1024 - samplesPerFrame  --Não queremos menos que 1024 samples por frame, entao usaremos offset 

  let total = duração * round framerate

  let rangeFrames = [0..total]

  _ <- putStrLn $ printf "%dx%d - %.f fps" width height framerate
  _ <- putStrLn $ printf "Sample Count: %d" samples
  _ <- putStrLn $ printf "SampleRate: %d\nduracao em segundos: %d\nNumero de frames: %d" sampleRate duração total
  _ <- putStrLn $ printf "cada frame representará %d + %d offset samples" samplesPerFrame offsetSamples


  {- Chamamos essa função para retornar uma lista de informações úteis para
     desenharmos o mandelbrot reagindo a música.
     [((a, b), c)] onde a é o numero do frame
     b é quantidade de grave que o fft disse que tem no frame atual
     c é a versão suavizada de b, objetivo dela é não ter picos extremos -}
  let dbList = getAnimationTimings waveSmp rangeFrames samplesPerFrame duração


  _ <- putStr "Deseja salvar em vídeo? [Y/N] "
  opt <- getLine

  _ <- if null opt || 'y' /= toLower (head opt) && 'n' /= toLower (head opt)
    then exitFalha "Não entendi. Abortando."
    else putStrLn (if 'y' == toLower (head opt) then "Saída de vídeo" else "Saída em imagens")

  let opt1 = 'n' == toLower (head opt)

  if opt1
    then mapM_ (\(n,x) -> doAnimSave x (genPath $ truncate n) estático >> status total n) $ zip [1..] dbList

    else writeVideo dbList samplesPerFrame sampleRate total

  putStrLn "\nDone"

