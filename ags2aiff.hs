{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString.Lazy as B
import Data.Serialize hiding (get, getBytes)
import Data.Serialize.Builder (toLazyByteString)
import qualified Data.Map as Map
import qualified Control.Monad.State as State
import Control.Monad
import Data.Word (Word8, Word16, Word32)
import Data.List (intercalate, mapAccumL)
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import Data.Monoid (Monoid(..))
import Data.Foldable (foldMap)
import System.Console.CmdArgs
import Prelude hiding (all)


data Args = Args { all :: Bool, path :: String } deriving (Show, Data, Typeable)

argsDef :: Args
argsDef = Args 
  { all = False &= help "Combine all generated samples in one audio file"
  , path = "./" &= argPos 0 &= typDir
  } &= versionArg [ignore] &= summary "ags2aiff v0.1, Sjoerd Visscher 2010"


sampleRate :: Int
sampleRate = 24000

main :: IO ()
main = do 
  args <- cmdArgs argsDef
  let p = path args ++ (if last (path args) == '/' then "" else "/")
  files <- getDirectoryContents p
  let agsFiles = filter ((== ".ags") . reverse . take 4 . reverse) files
  sounds <- mapM (generateSound . (p ++)) agsFiles
  if (all args) 
    then do
      B.writeFile (p ++ "Sound.aiff") $ generateAIFF (foldMap (`mappend` silence1s) sounds)
      writeFile (p ++ "Sound.js") $ generateJS sounds agsFiles
    else do
      flip mapM_ (zip sounds agsFiles) $ \(sound, fname) ->
        B.writeFile ((++ ".aiff") . reverse . drop 4 . reverse $ fname) $ generateAIFF sound

generateSound :: String -> IO Audio
generateSound path = do
  contents <- liftM B.unpack $ B.readFile path
  let offsets = State.evalState getOffsets contents
  let notes = map (State.evalState getNotes . flip drop (contents ++ repeat 0)) offsets
  return . zipAdd3 . map (foldMap sample) $ notes



data Note = Note { duration :: Int, freq :: Int, attenuation :: Int } deriving Show

-- First field is length, second field is an infinitly long sample.
data Audio = Audio !Int [Word16]

instance Monoid Audio where
  mempty = Audio 0 (repeat 0)
  Audio l1 d1 `mappend` Audio l2 d2 = Audio (l1 + l2) (take l1 d1 ++ d2)

silence1s :: Audio
silence1s = Audio sampleRate (repeat 0)

sample :: Note -> Audio
sample (Note dur f a) = Audio len (dataMap Map.! (f, a `mod` 16))
  where 
    len = (dur * sampleRate) `div` 60

dataMap :: Map.Map (Int, Int) [Word16]
dataMap = Map.fromList [((f, a), createData (111860 / fromIntegral f) ((15 - fromIntegral a) / 15)) | f <- [0..64*16], a <- [0..15]]

createData :: Double -> Double -> [Word16]
createData freq vol =
  [round $ vol * exp (i * decayMul) * max (-10000) (min 10000 (shapeMul * sin (i * freqMul))) | i <- [0..]]
    where
      shapeMul :: Double
      shapeMul = 200000000 / 3 / freq
      freqMul :: Double
      freqMul = freq * 2 * pi / fromIntegral sampleRate
      decayMul :: Double
      decayMul = (-4) / fromIntegral sampleRate

zipAdd3 :: [Audio] -> Audio
zipAdd3 [Audio lx xs, Audio ly ys, Audio lz zs] = Audio (maximum [lx, ly, lz]) (zipWith3 (\x y z -> x + y + z) xs ys zs)



getOffsets :: State.State [Word8] [Int]
getOffsets = replicateM 3 get2

getNote :: State.State [Word8] Note
getNote = liftM3 Note get2 getFrequency get1

getNotes :: State.State [Word8] [Note]
getNotes = do
  note <- getNote
  if (duration note == 0xffff) then return [] else do
    notes <- getNotes
    return (note : notes)

getFrequency :: State.State [Word8] Int
getFrequency = do
  b1 <- get1
  b2 <- get1
  return ((b1 `mod` 64) * 16 + (b2 `mod` 16))
  
get1 :: State.State [Word8] Int
get1 = do
  (b:rest) <- State.get
  State.put rest
  return (fromEnum b)

get2 :: State.State [Word8] Int
get2 = liftM2 (+) get1 (liftM (256 *) get1)





generateAIFF :: Audio -> B.ByteString
generateAIFF (Audio len samples) = toLazyByteString . execPut $ chunk "FORM" (36 + len * 2) (putAscii "AIFF" >> commonChunk >> soundDataChunk) where
  commonChunk    = chunk "COMM" 18 (short 1 >> long (fromIntegral len) >> short 16 >> ext (fromIntegral sampleRate))
  soundDataChunk = chunk "SSND" (8 + len * 2) (long 0 >> long 0 >> (mapM_ short $ take len samples))

chunk :: String -> Int -> Put -> Put
chunk ckId len ckData = putAscii ckId >> (long . fromIntegral $ len) >> ckData

putAscii :: String -> Put
putAscii = mapM_ (putWord8 . fromIntegral . fromEnum)

long :: Putter Word32
long = putWord32be

short :: Putter Word16
short = putWord16be

ext :: Double -> Put
ext _ = mapM_ putWord8 [0x40, 0x0D, 0xBB, 0x80, 0, 0, 0, 0, 0, 0]



generateJS :: [Audio] -> [String] -> String
generateJS audios names = "{" ++ intercalate "," parts ++ "}"
  where
    parts = snd $ mapAccumL (
        \pos (Audio len _, name) -> 
          let lenInS = fromIntegral len / fromIntegral sampleRate in
          let num = reverse . drop 4 . reverse . drop 5 $ name in
            (pos + lenInS + 1, "\"" ++ num ++ "\":[" ++ (take 6 $ show pos) ++ "," ++ (take 6 $ show lenInS) ++ "]")
      ) (0 :: Double) (zip audios names)
