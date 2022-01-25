{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Maybe
import System.Directory

newtype BoW = BoW {bowToMap:: M.Map T.Text Int} deriving (Show, Read)
wordtoBoW :: T.Text -> BoW
wordtoBoW w = BoW $ M.fromList [(w,1)]
emptyBoW = BoW M.empty

textToBoW :: T.Text -> BoW
textToBoW = foldMap wordtoBoW . T.words 

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

bowFromFolder :: FilePath -> IO BoW
bowFromFolder folderPath = do
    filenames <- listDirectory folderPath
    bows <- mapM (\filename -> bowfromFile (folderPath <> filename)) filenames
    return $ fold bows

spamBow :: IO BoW
spamBow = bowFromFolder "app\\data\\spam\\"

hamBow :: IO BoW
hamBow = bowFromFolder "app\\data\\ham\\"


main :: IO ()
main = putStrLn "Hello, Haskell!"
