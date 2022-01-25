{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Maybe
import Data.Char
import Data.List
import Data.Function
import System.Directory
-- import Control.Monad
import Text.Printf

newtype Word' = Word' T.Text deriving (Show,Read,Ord)

mkWord:: T.Text -> Word
mkWord = Word' . T.toUpper

newtype BoW = BoW 
    {bowToMap:: M.Map Word Int} deriving (Show, Read)

summaryBoW::BoW -> IO ()
summaryBoW (BoW bow)= do
    forM_ (sortBy (compare `on` snd) $ M.toList bow) $ \(w,f) -> printf "%s -> %d \n" w f


normalizeTextToWords:: T.Text -> [Word']
normalizeTextToWords = map mkWord . T.words . T.map (\x -> if isAlphaNum x then x else ' ')

wordtoBoW :: T.Text -> BoW
wordtoBoW w = BoW $ M.fromList [(w,1)]
emptyBoW = BoW M.empty

textToBoW :: T.Text -> BoW
textToBoW = foldMap wordtoBoW . normalizeTextToWords 

--Union with numbers is sum, with strings concatenate
instance Semigroup BoW where
    (<>) (BoW bow1) (BoW bow2) = BoW $ M.unionWith (+) bow1 bow2

instance Monoid BoW where
    mempty = emptyBoW

--M.tolist returns tuples of (key,value) so snd extracts the value!
wordsCount :: BoW -> Int
wordsCount (BoW bow) = sum $ map snd $ M.toList bow

--M.lookup returns a Maybe!!
wordProbability :: T.Text -> BoW -> Float
wordProbability w bow = fromIntegral n/ (fromIntegral $ wordsCount bow) where
    n = fromMaybe 0 $ M.lookup w $ bowToMap bow

-- <$> functor operator, gives me the thing inside the Monad!s
bowfromFile :: FilePath -> IO BoW
bowfromFile filepath =
     textToBoW <$> T.readFile filepath
--listDirectory returns [filenames] !!
bowFromFolder :: FilePath -> IO BoW
bowFromFolder folderPath = do
    filenames <- listDirectory folderPath
    bows <- mapM (\filename -> bowfromFile (folderPath <> filename)) filenames
    return $ fold bows

spamBow :: IO BoW
spamBow = bowFromFolder "app\\data\\train\\spam\\"

hamBow :: IO BoW
hamBow = bowFromFolder "app\\data\\train\\ham\\"

wordProbabilitySpam::T.Text->IO Float
wordProbabilitySpam w = do         
    pws <- wordProbability w <$> spamBow
    phs <- wordProbability w <$> hamBow
    let ps = pws + phs
    return $ if ps == 0 then 0.0 else pws/ps

textProbabilitySpam::T.Text->IO Float
textProbabilitySpam text=do
    let ws = normalizeTextToWords text
    ps <- mapM wordProbabilitySpam ws
    let ips = map (\p -> 1.0-p) ps
    let pp = product ps
    return (pp/(pp + product ips))

main :: IO ()
main = putStrLn "Hello, Haskell!"
