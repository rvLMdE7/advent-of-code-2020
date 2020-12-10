{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL


type Parser = P.Parsec Void T.Text

type Chain = Seq Int
type Joltages = Set Int

main :: IO ()
main = do
    file <- readFileUtf8 "day-10/input.txt"
    case P.parse (parseJoltages <* P.eof) "day 10 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            print $ part1 input

part1 :: Joltages -> Either String Int
part1 jolts = if validChain chain
    then Right (ones * threes)
    else Left $ [Printf.s|not a valid chain: %?|] chain
  where
    chain = makeChain jolts
    hist = makeHistogram $ linkLengths chain
    ones = M.findWithDefault 0 1 hist
    threes = M.findWithDefault 0 3 hist

makeChain :: Joltages -> Chain
makeChain = Set.toAscList .> Seq.fromList

validChain :: Chain -> Bool
validChain = getLinks .> validLinks
  where
    validLinks = all validLink
    validLink (a, b) = between a (a + 3) b

getLinks :: Chain -> Seq (Int, Int)
getLinks chain = Seq.zip (bot :<| chain) (chain :|> top)
  where
    bot = 0
    len = Seq.length chain
    top = fromMaybe bot (Seq.lookup (len - 1) chain) + 3

linkLengths :: Chain -> M.Map Int Int
linkLengths chain = makeHistogram lens
  where
    links = getLinks chain
    lens = uncurry subtract <$> links

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseJoltages :: Parser Joltages
parseJoltages = parseJoltage `sepEndByUniq` PC.newline

parseJoltage :: Parser Int
parseJoltage = PC.space *> PCL.decimal

sepEndByUniq :: (Show a, Ord a) => Parser a -> Parser b -> Parser (Set a)
sepEndByUniq parser sep = go Set.empty
  where
    go acc = P.optional parser >>= \case
        Nothing -> pure acc
        Just x -> if x `Set.member` acc
            then fail $ [Printf.s|repeated value %?|] x
            else do
                let newAcc = Set.insert x acc
                more <- P.option False (sep $> True)
                if more then go newAcc else pure newAcc

makeHistogram :: (Foldable t, Ord a) => t a -> M.Map a Int
makeHistogram = foldr insert M.empty
  where
    insert x = M.insertWith (+) x 0

between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)
