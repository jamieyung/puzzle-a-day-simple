module Piece where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as A
import Data.Int.Bits as Bits
import Data.Newtype (class Newtype)

newtype Piece = Piece Int

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

derive instance Newtype Piece _
derive newtype instance Show Piece
derive newtype instance Eq Piece
derive newtype instance Ord Piece
