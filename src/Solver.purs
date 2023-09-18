module Solver where

import Prelude

import Board (Board(..))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..), fromMaybe)
import Piece (Piece(..))
import Piece as Piece

step :: Array Piece -> Board -> Board
step pieces b = fromMaybe b $ A.head $ A.sort $ A.catMaybes $ mapWithIndex f pieces
  where
  f :: Int -> Piece -> Maybe Board
  f i p = do
    rest <- A.deleteAt i pieces -- remove the current piece from the array
    p
      # Piece.genPiecePlacements -- generate all possible placements for the current piece
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
