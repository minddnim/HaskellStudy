{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Control.Monad (mzero,(=<<),(<=<),(>=>))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Attoparsec as AP (Result(..),parseOnly)
import qualified Data.Attoparsec.Number as N (Number(I,D))
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)

data BmsTable = BmsTable {
  btComment :: String,
  btTitle :: String,
  btUrl :: String,
  btArtist :: String,
  btNameDiff :: String,
  btUrlDiff :: String,
  btLevel :: String,
  btLr2Bms :: Int,
  btMd5 :: String
} deriving (Show, Eq)

instance FromJSON BmsTable where
  parseJSON (Object v) = BmsTable
                         <$> v .: "comment"
                         <*> v .: "title"
                         <*> v .: "url"
                         <*> v .: "artist"
                         <*> v .: "name_diff"
                         <*> v .: "url_diff"
                         <*> v .: "level"
                         <*> v .: "lr2_bmsid"
                         <*> v .: "md5"
  parseJSON _ = mzero

parseBmsTable :: Value -> Result BmsTable
parseBmsTable = fromJSON

value :: Value
value = 
  object["comment"   .= ("" :: String),
         "title"     .= ("naughty girl@Excellency's Palace Normal" :: String),
         "url"       .= ("http://www.nicovideo.jp/watch/sm24015862" :: String),
         "artist"    .= ("DJ Haruka MAD Izm*" :: String),
         "name_diff" .= ("" :: String),
         "url_diff"  .= ("" :: String),
         "level"     .= ("-1" :: String),
         "lr2_bmsid" .= (191143 :: Int),
         "md5"       .= ("23ef3ddb6a9fc15f5a6d3313d62418d5" :: String)
  ]

main = print $ parseBmsTable value

