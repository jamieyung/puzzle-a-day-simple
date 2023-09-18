module Main where

import Prelude

import Board (Board(..))
import Board as Board
import Data.Int (binary, toStringAs)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)
import Piece (Piece(..))
import Solver as Solver
import Util (fromBinaryString)

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
    answer = Solver.step pieces board
  log $ "Board: " <> Board.print board
  log $ "Pieces: " <> show (map (unwrap >>> toStringAs binary) pieces)
  log $ "\nAnswer:\n" <> Board.print answer
  log $ "\nAnswer (show):\n" <> show answer
  log $ "\nAnswer (debug):\n" <> Board.debugPrint answer
