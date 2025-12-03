-- module

module AOC.MD5 where


-- import

import "this" Prelude

import Crypto.Hash.MD5        (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8  (pack, unpack)
import Data.List              (foldl')


-- md5

hashMD5 :: String -> String
hashMD5 = hashMD5s 1

hashMD5s :: Int -> String -> String
hashMD5s steps input = unpack $ foldl' (const . encode . hash) (pack input) [1..steps]
