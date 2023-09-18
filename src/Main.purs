module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (intercalate)
import Data.Array as A
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (binary, fromStringAs, toStringAs)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as CU
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let
    board = Board { b: fromBinaryString "00000001", pieces: [] }
    pieces =
      [ Piece $ fromBinaryString "111"
      , Piece $ fromBinaryString "111"
      , Piece $ fromBinaryString "1"
      , Piece $ fromBinaryString "1"
      , Piece $ fromBinaryString "1"
      , Piece $ fromBinaryString "1"
      , Piece $ fromBinaryString "1111111"
      ]
    answer = step pieces board
  log $ "Board: " <> print board
  log $ "Pieces: " <> show (map (unwrap >>> toStringAs binary) pieces)
  log $ "\nAnswer:\n" <> print answer
  log $ "\nAnswer (show):\n" <> show answer
  log $ "\nAnswer (debug):\n" <> debugPrint answer

fromBinaryString :: String -> Int
fromBinaryString = fromStringAs binary >>> fromMaybe 0

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

step :: Array Piece -> Board -> Board
step pieces b = fromMaybe b $ A.head $ A.sort $ A.catMaybes $ mapWithIndex f pieces
  where
  f :: Int -> Piece -> Maybe Board
  f i p = do
    rest <- A.deleteAt i pieces -- remove the current piece from the array
    p
      # genPiecePlacements -- generate all possible placements for the current piece
      # map (valid b) -- for each possibility, try placing it on the board
      # A.catMaybes -- filter out the invalid ones
      # map (step rest) -- recurse with the remaining pieces and the new boards
      # A.sort -- sort (placing the best board at the front of the list)
      # A.head -- return the best board

valid :: Board -> Piece -> Maybe Board
valid (Board { b, pieces }) piece@(Piece p) =
  if Bits.and b p == 0 then
    Just $ Board { b: Bits.or b p, pieces: A.snoc pieces piece }
  else
    Nothing

newtype Board = Board { b :: Int, pieces :: Array Piece }

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

newtype Piece = Piece Int

derive instance Newtype Piece _
derive newtype instance Show Piece
derive newtype instance Eq Piece
derive newtype instance Ord Piece

-- Assumes that `p` is shifted all the way to the right.
genPiecePlacements :: Piece -> Array Piece
genPiecePlacements (Piece p) = tailRec go { arr: [], cur: p }
  where
  go z =
    if z.cur >= (Bits.shl 1 8) then Done z.arr
    else Loop
      { arr: A.cons (Piece z.cur) z.arr
      , cur: Bits.shl z.cur 1
      }

toBitString :: Int -> String
toBitString = toStringAs binary >>> padStart 8 '0'

foreign import padStart :: Int -> Char -> String -> String
