{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CsvDataLoader(Email(..),loadCsv) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.String
import Data.Either as E
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad

-- Message ID,Subject,Message,Spam/Ham,Date
data Email = Email { message_ID :: !T.Text , subject :: !T.Text, message :: !T.Text, spamHam :: !T.Text, date :: !T.Text}
    deriving (Show, Eq, Ord)

instance FromNamedRecord Email where
    parseNamedRecord m = Email <$> m .: "message_ID" <*> m .: "subject" <*> m .: "message" <*> m .: "spamHam" <*> m .: "date"

instance ToNamedRecord Email where
    toNamedRecord (Email message_ID  subject message spamHam date ) = namedRecord [
        "message_ID".=message_ID , "subject".=subject, 
        "message".=message, "spamHam".=spamHam, "date".=date]

instance DefaultOrdered Email where
    headerOrder _ = header ["message_ID", "subject", "message", "spamHam", "date"]

loadCsv :: IO (Maybe [Email])
loadCsv = do
    csvData <- BL.readFile "app\\data\\enron_spam_data.csv"
    case decodeByName csvData of
        Left err -> return Nothing
        Right (_,records) -> return $ Just $ foldl (\acc elem -> (:) elem acc) [] records
