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
    { one :: Int
    , two :: Int
    , letter :: Char
    } deriving (Bounded, Eq, Ord, Show, Read)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-02/input.txt"
    print $ part1 input
    print $ part2 input

part1 :: [(Policy, T.Text)] -> Int
part1 = filter (uncurry isValidPwdUnofficial) .> length

isValidPwdUnofficial :: Policy -> T.Text -> Bool
isValidPwdUnofficial policy pwd =
    (one policy <= numTimes) && (numTimes <= two policy)
  where
    numTimes = T.count neededStr pwd
    neededStr = T.singleton $ letter policy

part2 :: [(Policy, T.Text)] -> Int
part2 = filter (uncurry isValidPwdOfficial) .> length

isValidPwdOfficial :: Policy -> T.Text -> Bool
isValidPwdOfficial policy pwd =
   (charOne == letter policy) `xor` (charTwo == letter policy)
  where
    charOne = T.index pwd (one policy - 1)
    charTwo = T.index pwd (two policy - 1)

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> [(Policy, T.Text)]
parseInput = T.lines .> fmap parse
  where
    parse = P.parseMaybe parsePolicyAndPwd .> fromJust

parsePolicyAndPwd :: Parser (Policy, T.Text)
parsePolicyAndPwd = do
    x <- read <$> (PC.space *> P.some PC.digitChar)
    PC.space *> PC.char '-'
    y <- read <$> (PC.space *> P.some PC.digitChar)
    char <- PC.space *> P.anySingle
    PC.space *> PC.char ':'
    pwd <- PC.space *> P.many PC.alphaNumChar
    pure (MkPolicy x y char, T.pack pwd)
