{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Maybe
import Data.Char
import qualified Data.List as L
import Data.Function
import System.Directory
import Data.Csv
import qualified CsvDataLoader as CL
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
    forM_ (L.sortBy (compare `on` snd) $ M.toList bow) $ \(w,f) -> printf "%s -> %d \n" (wordToText w) f


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

getTrainEmails::IO [CL.Email]
getTrainEmails = do 
    emails <- fromJust <$> CL.loadCsv
    let n = round $ 0.7 * (fromIntegral $ L.length emails)
    return $ take n emails

getMailBySpamHam::T.Text -> IO [CL.Email]
getMailBySpamHam spamOrHam = filteredEmails where
    filteredEmails = filter (\(CL.Email message_ID subject message spamHam date)-> spamHam==spamOrHam) <$> getTrainEmails

bowFromEmail::CL.Email -> BoW
bowFromEmail (CL.Email message_ID subject message spamHam date) = textToBoW message

bowFromEmails:: T.Text -> IO BoW
bowFromEmails spamOrHam = do
    -- emails <- getMailBySpamHam spamOrHam 
    bows <- map bowFromEmail <$> getMailBySpamHam spamOrHam
    return $ fold bows

data SpamModel = SpamModel {spamBow:: BoW,
                            hamBow::BoW} deriving Show

spamModel :: IO SpamModel
spamModel = do
    spam <- bowFromEmails "spam"
    ham <- bowFromEmails "ham"
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

main::IO ()
main = do
    sm@(SpamModel spam ham) <- spamModel
    summaryBoW spam
