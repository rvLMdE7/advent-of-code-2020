module Day02 where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Flow ((.>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Data.Void (Void)
import Data.Maybe (fromJust)


type Parser = P.Parsec Void T.Text

data Policy = MkPolicy
    { minTimes :: Int
    , maxTimes :: Int
    , letter :: Char
    } deriving (Bounded, Eq, Ord, Show, Read)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-02/input.txt"
    print $ part1 input

part1 :: [(Policy, T.Text)] -> Int
part1 = filter (uncurry isValidPwd) .> length

isValidPwd :: Policy -> T.Text -> Bool
isValidPwd policy pwd =
    (minTimes policy <= numTimes) && (numTimes <= maxTimes policy)
  where
    numTimes = T.count neededStr pwd
    neededStr = T.singleton $ letter policy

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> [(Policy, T.Text)]
parseInput = T.lines .> fmap parse
  where
    parse = P.parseMaybe parsePolicyAndPwd .> fromJust

parsePolicyAndPwd :: Parser (Policy, T.Text)
parsePolicyAndPwd = do
    minN <- read <$> (PC.space *> P.some PC.digitChar)
    PC.space *> PC.char '-'
    maxN <- read <$> (PC.space *> P.some PC.digitChar)
    char <- PC.space *> P.anySingle
    PC.space *> PC.char ':'
    pwd <- PC.space *> P.many PC.alphaNumChar
    pure (MkPolicy minN maxN char, T.pack pwd)
