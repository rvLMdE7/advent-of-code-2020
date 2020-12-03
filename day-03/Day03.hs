{-# Language QuasiQuotes #-}

module Day03 where

import Data.ByteString qualified as B
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf


data Tile = Open | Tree
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Grid a = V.Vector (V.Vector a)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-03/input.txt"
    print $ part1 (3, 1) input
    print $ part2 [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] input

part1 :: (Int, Int) -> Grid Tile -> Int
part1 slope grid =
    length $ filter (== Tree) $ trailAtSlopeTopToBotHorizLoop slope grid

part2 :: [(Int, Int)] -> Grid Tile -> Int
part2 slopes grid = product [part1 slope grid | slope <- slopes]

trailAtSlopeTopToBotHorizLoop :: (Int, Int) -> Grid a -> [a]
trailAtSlopeTopToBotHorizLoop slope grid = go (0, 0) []
  where
    rows = V.length grid
    go pos acc
        | snd pos < rows = go (pos `addPt` slope) (index pos grid : acc)
        | otherwise = reverse acc

index :: (Int, Int) -> Grid a -> a
index (x, y) grid = grid V.! r V.! c
  where
    rows = V.length grid
    cols = V.length $ V.head grid
    c = x `mod` cols
    r = y `mod` rows

addPt :: Num a => (a, a) -> (a, a) -> (a, a)
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> Grid Tile
parseInput txt =
    txt & T.lines
        & V.fromList
        & fmap (T.unpack .> V.fromList .> fmap charToTile)
  where
    charToTile c = case c of
        '.' -> Open
        '#' -> Tree
        _ -> error $ [Printf.s|unexpected char %c in input %Q|] c txt
