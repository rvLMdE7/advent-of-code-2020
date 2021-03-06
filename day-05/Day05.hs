{-# LANGUAGE QuasiQuotes #-}

module Day05 where

import Control.Monad (replicateM, guard)
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC


type Parser = P.Parsec Void T.Text

data Row = F | B
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Col = L | R
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data BoardingPass = MkBoardingPass
    { bpRows :: [Row]
    , bpCols :: [Col]
    } deriving (Eq, Ord, Read, Show)

data Seat = MkSeat
    { stRow :: Int
    , stCol :: Int
    } deriving (Bounded, Eq, Ord, Read, Show)

main :: IO ()
main = do
    file <- readFileUtf8 "day-05/input.txt"
    case P.parse (parseBoardingPasses <* P.eof) "day 5 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            print $ part1 input
            either putStrLn print $ part2 input

part1 :: [BoardingPass] -> Int
part1 = fmap (passToSeat .> seatToId) .> maximum

part2 :: [BoardingPass] -> Either String Int
part2 passes = case findHoles $ fmap (passToSeat .> seatToId) passes of
    [seatId] -> Right seatId
    seatIds -> Left $
        [Printf.s|expected one hole, but instead found %i holes: %?|]
            (length seatIds) seatIds

findHoles :: [Int] -> [Int]
findHoles xs = do
    x <- if null xs then [] else [minimum xs .. maximum xs]
    guard $ member (x - 1) && notMember x && member (x + 1)
    pure x
  where
    member = flip S.member $ S.fromList xs
    notMember = flip S.notMember $ S.fromList xs

seatToId :: Seat -> Int
seatToId (MkSeat row col) = (row * 8) + col

passToSeat :: BoardingPass -> Seat
passToSeat (MkBoardingPass rows cols) = MkSeat
    { stRow = evalAsBinary (rowsToBin rows)
    , stCol = evalAsBinary (colsToBin cols)
    }
  where
    rowsToBin = let rowToBin = fromEnum in fmap rowToBin
    colsToBin = let colToBin = fromEnum in fmap colToBin

evalAsBinary :: [Int] -> Int
evalAsBinary = reverse .> zip [0..] .> fmap toVal .> sum
  where
    toVal :: (Int, Int) -> Int
    toVal (pos, digit) = (2 ^ pos) * digit

parseBoardingPasses :: Parser [BoardingPass]
parseBoardingPasses = P.sepEndBy parseBoardingPass (PC.char '\n')

parseBoardingPass :: Parser BoardingPass
parseBoardingPass = do
    rows <- replicateM 7 row
    cols <- replicateM 3 col
    pure $ MkBoardingPass rows cols
  where
    row = P.choice [PC.char 'F' $> F, PC.char 'B' $> B]
    col = P.choice [PC.char 'L' $> L, PC.char 'R' $> R]

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
