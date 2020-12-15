{-# LANGUAGE LambdaCase #-}

module Day15 where

import Control.Monad (replicateM_)
import Control.Monad.State (State, get, modify, execState)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq((:||>)), (|>))
import Data.Sequence.NonEmpty qualified as Seq.NE


main :: IO ()
main = do
    print $ part1 (2 :| [20, 0, 4, 1, 17])

part1 :: NonEmpty Int -> Int
part1 input = playGame input (2020 - length input) & Seq.NE.last

playGame :: NonEmpty Int -> Int -> NESeq Int
playGame input n = execState (takeTurns n) (Seq.NE.fromList input)

takeTurns :: Int -> State (NESeq Int) ()
takeTurns n = replicateM_ n takeTurn

takeTurn :: State (NESeq Int) ()
takeTurn = get >>= \case
    xs :||> x -> do
        let age = maybe 0 (length xs -) (Seq.elemIndexR x xs)
        modify (|> age)
