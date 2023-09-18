module Board where

import Prelude

import Data.Array (intercalate)
import Data.Array as A
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int.Bits as Bits
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as CU
import Piece (Piece)
import Util (toBitString)

newtype Board = Board { b :: Int, pieces :: Array Piece }

print :: Board -> String
print b@(Board { pieces }) = CU.fromCharArray $ foldrWithIndex go init pieces
  where
  init = CU.toCharArray $ toBitString $ original b

  -- wherever a '1' appears in the piece bitboard, set the corresponding index
  -- of `z` to the char given by `asciiCharIndex`. Effectively stamps the given
  -- piece onto the string using a given char.
  go :: Int -> Piece -> Array Char -> Array Char
  go asciiCharIndex p z = foldrWithIndex f z pieceArr
    where
    pieceArr = p # unwrap # toBitString # CU.toCharArray
    char = fromMaybe '.' $ fromCharCode $ 65 + asciiCharIndex
    f i c z1 = if c /= '0' then fromMaybe z1 $ A.updateAt i char z1 else z1

debugPrint :: Board -> String
debugPrint board@(Board { b, pieces }) = intercalate "\n" $
  [ "original board: " <> (toBitString $ original board) ]
    <> (mapWithIndex (\i p -> "Piece " <> show i <> ":        " <> toBitString (unwrap p)) pieces)
    <> [ "filled board:   " <> toBitString b ]

-- returns the original board without the pieces on it.
original :: Board -> Int
original (Board { b, pieces }) = A.foldr Bits.xor b (map unwrap pieces)

derive instance Newtype Board _
derive newtype instance Show Board
derive newtype instance Eq Board
instance Ord Board where
  compare (Board x) (Board y) =
    if A.length x.pieces /= A.length y.pieces then
      -- sort boards with more pieces closer to the start of the list
      compare (A.length y.pieces) (A.length x.pieces)
    else
      -- otherwise arbitrarily pick the board with pieces closer to the front
      -- of the board, aka. boards with more bits in higher positions
      compare y.b x.b
