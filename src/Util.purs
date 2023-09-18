module Util where

import Prelude

import Data.Int (binary, fromStringAs, toStringAs)
import Data.Maybe (fromMaybe)

fromBinaryString :: String -> Int
fromBinaryString = fromStringAs binary >>> fromMaybe 0

toBitString :: Int -> String
toBitString = toStringAs binary >>> padStart 8 '0'

foreign import padStart :: Int -> Char -> String -> String
