{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Aeson
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as C

data TBResponse = TBResponse
  { status :: Int,
    message :: String,
    tbData :: String,
    readingLvl :: Float,
    readingTime :: Float
  }

instance ToJSON TBResponse where
  toJSON (TBResponse s m d rl rt) = object ["status" .= s, "message" .= m, "data" .= d, "reading_lvl" .= rl, "reading_time" .= rt]

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (writeBS "hello world") <|>
  route [ ("tidbit", method POST tidbit)
        ] 

tidbit :: Snap ()
tidbit = do
  bodyBytes <- readRequestBody 2048
  let body = (C.unpack bodyBytes)
  let res = encode (TBResponse {status=200, message="test", tbData="dataTest", readingLvl=9.0, readingTime=2.0})
  writeLBS res
