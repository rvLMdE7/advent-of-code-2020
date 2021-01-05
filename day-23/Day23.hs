module Day23 where

import Control.Monad (replicateM_)
import Control.Monad.State (State, get, gets, put, modify, execState)
import Data.Char qualified as Char
import Data.Foldable qualified as Fold
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Flow ((.>))


main :: IO ()
main = do
    let input = "952438716"
    putStrLn $ run part1 input
  where
    run part = toSeq .> part .> fromSeq
    toSeq = fmap Char.digitToInt .> Seq.fromList
    fromSeq = Fold.toList .> fmap Char.intToDigit

part1 :: Seq Int -> Seq Int
part1 = execState $ do
    moves 100
    one <- gets $ Seq.elemIndexL 1 .> fromJust
    modify $ rotateL one .> Seq.drop 1

moves :: Int -> State (Seq Int) ()
moves n = replicateM_ n move

move :: State (Seq Int) ()
move = do
    start <- get
    let label = start `Seq.index` 0
    pick <- extractCups n
    dest <- getDest label
    let off = if dest `mod` (length start - n) == 0 then n else 0
    modify $ insert pick dest .> rotateL (1 + off)
  where
    n = 3

extractCups :: Int -> State (Seq Int) (Seq Int)
extractCups j = do
    (out, new) <- gets $ extract (1, j)
    put new
    pure out

getDest :: Int -> State (Seq Int) Int
getDest label = do
    orig <- get
    let top = maximum orig
        bot = minimum orig
        run i = if i < bot
            then run top
            else case Seq.elemIndexL i orig of
                Nothing -> run (i - 1)
                Just j -> j + 1
    pure $ run (label - 1)

extract :: (Int, Int) -> Seq a -> (Seq a, Seq a)
extract (i, j) list = (left, rotateR i right)
  where
    (left, right) = Seq.splitAt j (rotateL i list)

rotateR :: Int -> Seq a -> Seq a
rotateR i = rotateL (negate i)

rotateL :: Int -> Seq a -> Seq a
rotateL i list
    | null list = list
    | otherwise = case Seq.splitAt (i `mod` length list) list of
        (left, right) -> right <> left

insert :: Seq a -> Int -> Seq a -> Seq a
insert new i orig = left <> new <> right
  where
    (left, right) = Seq.splitAt (i `mod` length orig) orig
