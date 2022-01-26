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

newtype Word' = Word' T.Text deriving (Show,Read,Eq,Ord)

mkWord:: T.Text -> Word'
mkWord = Word' . T.toUpper

wordToText:: Word'->T.Text
wordToText (Word' t) = t

newtype BoW = BoW 
    {bowToMap:: M.Map Word' Int} deriving (Show, Read)

summaryBoW::BoW -> IO ()
summaryBoW (BoW bow)= do
    forM_ (sortBy (compare `on` snd) $ M.toList bow) $ \(w,f) -> printf "%s -> %d \n" (wordToText w) f


normalizeTextToWords:: T.Text -> [Word']
normalizeTextToWords = map mkWord . T.words . T.map (\x -> if isAlphaNum x then x else ' ')

wordtoBoW :: Word' -> BoW
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
wordProbability ::  BoW -> Word' -> Float
wordProbability bow w = fromIntegral n/ (fromIntegral $ wordsCount bow) where
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

data SpamModel = SpamModel {spamBow:: BoW,
                            hamBow::BoW}

spamModel :: IO SpamModel
spamModel = do
    spam<-bowFromFolder "app\\data\\train\\spam\\"
    ham <-bowFromFolder "app\\data\\train\\ham\\"
    return $ SpamModel spam ham

seenWord::  Word' -> SpamModel -> Bool
seenWord w (SpamModel spamBow hamBow) = isJust sm || isJust hm 
    where
     sm = M.lookup w $ bowToMap spamBow
     hm = M.lookup w $ bowToMap hamBow
    
    
wordProbabilitySpam:: SpamModel -> Word' -> Maybe Float
wordProbabilitySpam sm@(SpamModel spamBow hamBow) w 
    | seenWord w sm = 
        let pws = wordProbability spamBow w 
            phs = wordProbability hamBow w 
            ps = pws + phs
        in Just (pws/ps)

    | otherwise = Nothing

textProbabilitySpam::SpamModel -> T.Text -> Float
textProbabilitySpam spammodel text = 
    let
     ws = normalizeTextToWords text
     ps = mapMaybe (wordProbabilitySpam spammodel) ws
     ips = map (\p -> 1.0-p) ps
     pp = product ps
    in pp/(pp + product ips)

main :: IO ()
main = putStrLn "Hello, Haskell!"
