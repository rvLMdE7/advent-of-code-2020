{-# LANGUAGE ScopedTypeVariables #-}

module Day13 where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL


type Parser = P.Parsec Void T.Text

main :: IO ()
main = do
    file <- readFileUtf8 "day-13/input.txt"
    case P.parse (parseInput1 <* P.eof) "day 13 input, part 1" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right (earliest, buses) -> print $ part1 earliest buses
    case P.parse (parseInput2 <* P.eof) "day 13 input, part 2" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right system -> print $ part2 system

part1 :: Int -> NE.NonEmpty Int -> Int
part1 earliest buses = bus * (time - earliest)
  where
    (bus, time) = minNextMult earliest buses

part2 :: NE.NonEmpty (Integer, Integer) -> Integer
part2 = solveCoprimes

solveCoprimes :: Integral a => NE.NonEmpty (a, a) -> a
solveCoprimes = foldr1 reduce .> fst
  where
    reduce (a1, n1) (a2, n2) = (solveCoprime (a1, n1) (a2, n2), n1 * n2)

solveCoprime :: Integral a => (a, a) -> (a, a) -> a
solveCoprime (a1, n1) (a2, n2) = (a1*m2*n2 + a2*m1*n1) `mod` (n1 * n2)
  where
    (m1, m2) = bezout n1 n2

bezout :: Integral a => a -> a -> (a, a)
bezout a b = go a b 1 0
  where
    go rOld r sOld s
        | r == 0    = (sOld, t)
        | otherwise = go r (rOld - q*r) s (sOld - q*s)
      where
        q = rOld `div` r
        t = if b == 0 then 0 else (rOld - sOld * a) `quot` b

minNextMult :: Int -> NE.NonEmpty Int -> (Int, Int)
minNextMult modulus =
    fmap (id &&& flip nextMult modulus) .> L.minimumBy (comparing snd)

nextMult :: Int -> Int -> Int
nextMult x modulus = x + modulus - (modulus `mod` x)

parseInput1 :: Parser (Int, NE.NonEmpty Int)
parseInput1 = do
    earliest <- PC.hspace *> PCL.decimal
    void $ PC.hspace *> PC.newline
    busIds <- catMaybes <$> P.sepBy1 busId (PC.hspace *> PC.char ',')
    PC.space
    if null busIds
        then fail "not enough inputs"
        else pure (earliest, NE.fromList busIds)
  where
    busId = P.choice
        [ Nothing <$ PC.char 'x'
        , Just <$> PCL.decimal
        ]

parseInput2 :: forall a. Integral a => Parser (NE.NonEmpty (a, a))
parseInput2 = do
    void $ PC.hspace *> (PCL.decimal :: Parser a)
    void $ PC.hspace *> PC.newline
    times <- P.sepBy1 time (PC.hspace *> PC.char ',')
    let maybeIndex may i = (const i &&& id) <$> may
        list = mapMaybe (uncurry maybeIndex) (zip times [0..])
    PC.space
    if null list
        then fail "not enough inputs"
        else pure (NE.fromList list)
  where
    time = P.choice
        [ Nothing <$ PC.char 'x'
        , Just <$> PCL.decimal
        ]

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
