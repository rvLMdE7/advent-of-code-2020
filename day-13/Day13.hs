module Day13 where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
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
    case P.parse (parseInput1 <* P.eof) "day 13 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right (earliest, buses) -> do
            print $ part1 earliest buses

part1 :: Int -> NE.NonEmpty Int -> Int
part1 earliest buses = bus * (time - earliest)
  where
    (bus, time) = minNextMult earliest buses

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

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
