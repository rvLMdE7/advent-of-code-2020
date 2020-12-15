{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Day15 where

import Control.Monad (replicateM_)
import Control.Monad.State (State, get, modify, execState)
import Data.Foldable qualified as Fold
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as List.NE
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq((:||>)), (|>))
import Data.Sequence.NonEmpty qualified as Seq.NE
import Flow ((.>))


data GameState = MkGameState
    { lastTurn :: {-# UNPACK #-} !Int
    , lastGo :: {-# UNPACK #-} !Int
    , memory :: !(IntMap Int)
    } deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
    let input = 2 :| [20, 0, 4, 1, 17]
    print $ part1 input
    print $ part2 input

part1 :: NonEmpty Int -> Int
part1 input = playGame input (2020 - length input) & Seq.NE.last

part2 :: NonEmpty Int -> Int
part2 input = playGame' input (30_000_000 - length input) & lastGo

playGame :: NonEmpty Int -> Int -> NESeq Int
playGame input n = execState (takeTurns n) (Seq.NE.fromList input)

playGame' :: NonEmpty Int -> Int -> GameState
playGame' input n = takeTurns' n initState
  where
    x :| xs = List.NE.reverse input
    vals = IntSet.fromList $ Fold.toList input
    lastSeen = (`Seq.NE.elemIndexR` Seq.NE.fromList input) .> fromJust
    initState = MkGameState
        { lastGo = x
        , lastTurn = length xs
        , memory = IntMap.fromSet lastSeen vals
        }

takeTurns :: Int -> State (NESeq Int) ()
takeTurns n = replicateM_ n takeTurn

takeTurns' :: Int -> GameState -> GameState
takeTurns' n !acc
    | n == 0 = acc
    | otherwise = takeTurns' (n - 1) (takeTurn' acc)

takeTurn :: State (NESeq Int) ()
takeTurn = get >>= \case
    xs :||> x -> do
        let age = maybe 0 (length xs -) (Seq.elemIndexR x xs)
        modify (|> age)

takeTurn' :: GameState -> GameState
takeTurn' (MkGameState !turn !go !mem) = MkGameState nTurn nGo nMem
  where
    nGo = maybe 0 (turn -) (IntMap.lookup go mem)
    nTurn = turn + 1
    nMem = IntMap.insert go turn mem
