{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified CsvDataLoader as CL
import Data.Char
import Data.Csv
import Data.Foldable
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
-- import Control.Monad
import Text.Printf

newtype Word' = Word' T.Text deriving (Show, Read, Eq, Ord)

mkWord :: T.Text -> Word'
mkWord = Word' . T.toUpper

wordToText :: Word' -> T.Text
wordToText (Word' t) = t

type Freq = Int

type Prob = Double

newtype BoW a = BoW
  {bowToMap :: M.Map Word' a}
  deriving (Show, Read)

summaryBoW :: (BoW Prob) -> IO ()
summaryBoW (BoW bow) = do
  forM_ (L.sortBy (compare `on` snd) $ M.toList bow) $ \(w, f) -> printf "%s -> %d \n" (wordToText w) f

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords = map mkWord . T.words . T.map (\x -> if isAlphaNum x then x else ' ')

wordtoBoW :: Word' -> BoW Freq
wordtoBoW w = BoW $ M.fromList [(w, 1)]

emptyBoW = BoW M.empty

textToBoW :: T.Text -> BoW Freq
textToBoW = foldMap wordtoBoW . normalizeTextToWords

-- Union with numbers is sum, with strings concatenate
instance Semigroup (BoW Freq) where
  (<>) (BoW bow1) (BoW bow2) = BoW $ M.unionWith (+) bow1 bow2

instance Monoid (BoW Freq) where
  mempty = emptyBoW

-- M.tolist returns tuples of (key,value) so snd extracts the value!
wordsCount :: (BoW Freq) -> Int
wordsCount (BoW bow) = sum $ map snd $ M.toList bow

-- M.lookup returns a Maybe!!
wordProbability :: (BoW Freq) -> Word' -> Prob
wordProbability bow w = fromIntegral n / (fromIntegral $ wordsCount bow)
  where
    n = fromMaybe 0 $ M.lookup w $ bowToMap bow

freqToProb :: BoW Freq -> BoW Prob
freqToProb bow_@(BoW bow) = BoW $ M.fromList $ zip words probs
  where
    probs = map ((wordProbability bow_) . fst) $ M.toList bow
    words = map fst $ M.toList bow

getTrainEmails :: IO [CL.Email]
getTrainEmails = do
  emails <- fromJust <$> CL.loadCsv
  let n = round $ 0.7 * (fromIntegral $ L.length emails)
  return $ take n emails

getValidationEmails :: IO [CL.Email]
getValidationEmails = do
  emails <- fromJust <$> CL.loadCsv
  let n = round $ 0.7 * (fromIntegral $ L.length emails)
  return $ snd $ splitAt n emails

getMailBySpamHam :: T.Text -> IO [CL.Email]
getMailBySpamHam spamOrHam = filteredEmails
  where
    filteredEmails = filter (\(CL.Email message_ID subject message spamHam date) -> spamHam == spamOrHam) <$> getTrainEmails

bowFromEmail :: CL.Email -> BoW Freq
bowFromEmail (CL.Email message_ID subject message spamHam date) = textToBoW message

bowFromEmails :: T.Text -> IO (BoW Freq)
bowFromEmails spamOrHam = do
  -- emails <- getMailBySpamHam spamOrHam
  bows <- map bowFromEmail <$> getMailBySpamHam spamOrHam
  return $ fold bows

data SpamModel = SpamModel
  { spamBow :: BoW Prob,
    hamBow :: BoW Prob
  }
  deriving (Show)

spamModel :: IO SpamModel
spamModel = do
  spam <- freqToProb <$> bowFromEmails "spam"
  ham <- freqToProb <$> bowFromEmails "ham"
  return $ SpamModel spam ham

dumpModelCSV :: Show a => BoW a -> FilePath -> IO ()
dumpModelCSV (BoW bow) filePath =
  writeFile filePath $
    unlines $
      (:) "word,\r\nprobability\r\n" $
        map (\((Word' w), value) -> printf "%s,%s" w (show value)) $
          M.toList bow

seenWord :: Word' -> SpamModel -> Bool
seenWord w (SpamModel spamBow hamBow) = isJust sm || isJust hm
  where
    sm = M.lookup w $ bowToMap spamBow
    hm = M.lookup w $ bowToMap hamBow

lookUpWordProbability :: BoW Prob -> Word' -> Prob
lookUpWordProbability bow w = fromMaybe 0 $ M.lookup w $ bowToMap bow

wordProbabilitySpam :: SpamModel -> Word' -> Maybe Prob
wordProbabilitySpam sm@(SpamModel spamBow hamBow) w
  | seenWord w sm =
      let pws = lookUpWordProbability spamBow w
          phs = lookUpWordProbability hamBow w
          ps = pws + phs
       in Just (pws / ps)
  | otherwise = Nothing

textProbabilitySpam :: SpamModel -> T.Text -> Prob
textProbabilitySpam spammodel text =
  let ws = normalizeTextToWords text
      ps = mapMaybe (wordProbabilitySpam spammodel) ws
      ips = map (\p -> 1.0 - p) ps
      pp = product ps
   in pp / (pp + product ips)

messageFromEmail :: CL.Email -> T.Text
messageFromEmail (CL.Email message_ID subject message spamHam date) = message

tagFromEmail :: CL.Email -> T.Text
tagFromEmail (CL.Email message_ID subject message spamHam date) = spamHam

classifySpamHam :: Prob -> T.Text
classifySpamHam x = if x >= 0.95 then "spam" else "ham"

dropFalse :: [Bool] -> [Bool]
dropFalse [] = []
dropFalse (x : xs)
  | x = x : dropFalse xs
  | otherwise = dropFalse xs

trainModel :: IO ()
trainModel = do
  (SpamModel spamBow hamBow) <- spamModel
  dumpModelCSV hamBow "app\\data\\hamModel.csv"
  dumpModelCSV spamBow "app\\data\\spamModel.csv"

spamModelFromCSV :: IO SpamModel
spamModelFromCSV = do
  spamWordProbs <- CL.loadModelCSV "app\\data\\spamModel.csv"
  hamWordProbs <- CL.loadModelCSV "app\\data\\hamModel.csv"
  let spamBow = BoW $ M.fromList $ map (\(CL.WordProb word prob) -> (mkWord word, prob)) $ fromJust spamWordProbs
  let hamBow = BoW $ M.fromList $ map (\(CL.WordProb word prob) -> (mkWord word, prob)) $ fromJust hamWordProbs
  return $ SpamModel spamBow hamBow

validateEmails :: Int -> IO ()
validateEmails n = do
  sm <- spamModelFromCSV

  emails <- take n <$> getValidationEmails

  let real_tags = map tagFromEmail emails
  let tags = map (classifySpamHam . (textProbabilitySpam sm) . messageFromEmail) emails
  putStrLn "Tags... \n"
  print $ tags

  putStrLn "Real Tags... \n"
  print $ tags

  let succeses = dropFalse [a == b | (a, b) <- zip tags real_tags]
  putStrLn "results... \n"
  print $ succeses

  putStrLn "ratio..."
  print $ ((fromIntegral $ L.length succeses) / (fromIntegral $ n))

main :: IO ()
main = do
  sm@(SpamModel spam ham) <- spamModel
  summaryBoW spam
