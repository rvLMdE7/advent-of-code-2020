{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Map qualified as M
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
type Joltages = Seq Int

main :: IO ()
main = do
    file <- readFileUtf8 "day-10/input.txt"
    case P.parse (parseJoltages <* P.eof) "day 10 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            print $ part1 input
            print $ part2 input

part1 :: Joltages -> Either String Int
part1 jolts = if validChain chain
    then Right (ones * threes)
    else Left $ [Printf.s|not a valid chain: %?|] chain
  where
    chain = makeChain jolts
    hist = linkLengths chain
    ones = M.findWithDefault 0 1 hist
    threes = M.findWithDefault 0 3 hist

part2 :: Joltages -> Int
part2 = Seq.sort .> divideIntoSubproblems .> fmap bruteForce .> product
  where
    bruteForce (a, xs, b) = bruteForceNumChains a b xs

bruteForceNumChains :: Int -> Int -> Seq Int -> Int
bruteForceNumChains bot top = bruteForceChains bot top .> Seq.length

bruteForceChains :: Int -> Int -> Seq Int -> Seq (Seq Int)
bruteForceChains bot top mid = go bot Seq.Empty (mid :|> top)
  where
    go :: Int -> Seq Int -> Seq Int -> Seq (Seq Int)
    go prev out inp = case inp of
        Seq.Empty -> Seq.singleton (bot :<| out)
        _ -> do
            (i, x) <- indexed $ Seq.takeWhileL (diffLE 3 prev) inp
            go x (out :|> x) (Seq.drop (i + 1) inp)

divideIntoSubproblems :: Seq Int -> Seq (Int, Seq Int, Int)
divideIntoSubproblems sequ = mapMaybe tryMakeSubproblem subSeqs
  where
    bot = 0
    top = fromMaybe bot (Seq.lookup (Seq.length sequ - 1) sequ) + 3
    subSeqs = groupByRolling (diffLT 3) (bot :<| (sequ :|> top))
    tryMakeSubproblem = \case
        x :<| (mid :|> y) -> Just (x, mid, y)
        _ -> Nothing

makeChain :: Joltages -> Chain
makeChain = Seq.sort

validChain :: Chain -> Bool
validChain = getLinks .> validLinks
  where
    validLinks = all validLink
    validLink (a, b) = between a (a + 3) b

getLinks :: Chain -> Seq (Int, Int)
getLinks chain = Seq.zip (bot :<| chain) (chain :|> top)
  where
    bot = 0
    top = 3 + fromMaybe bot (Seq.lookup (Seq.length chain - 1) chain)

linkLengths :: Chain -> M.Map Int Int
linkLengths chain = makeHistogram lens
  where
    links = getLinks chain
    lens = uncurry subtract <$> links

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseJoltages :: Parser Joltages
parseJoltages = Seq.fromList <$> P.sepEndBy parseJoltage PC.newline

parseJoltage :: Parser Int
parseJoltage = PC.space *> PCL.decimal

makeHistogram :: (Foldable t, Ord a) => t a -> M.Map a Int
makeHistogram = foldr insert M.empty
  where
    insert x = M.insertWith (+) x 1

between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)

diffLT :: (Ord a, Num a) => a -> a -> a -> Bool
diffLT x a b = diff a b < x

diffLE :: (Ord a, Num a) => a -> a -> a -> Bool
diffLE x a b = diff a b <= x

diff :: Num a => a -> a -> a
diff a b = abs (b - a)

indexed :: Seq a -> Seq (Int, a)
indexed s = Seq.fromList [0 .. Seq.length s - 1] `Seq.zip` s

groupByRolling :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupByRolling comp = go Seq.Empty
  where
    go acc = \case
        x :<| y :<| rest -> if comp x y
            then go (consBack x acc) (y :<| rest)
            else go (consBack x acc :|> Seq.Empty) (y :<| rest)
        x :<| Seq.Empty -> consBack x acc
        Seq.Empty -> acc

mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe f = \case
    x :<| xs -> case f x of
        Just res -> res :<| mapMaybe f xs
        Nothing -> mapMaybe f xs
    Seq.Empty -> Seq.Empty

consBack :: a -> Seq (Seq a) -> Seq (Seq a)
consBack x = \case
    rest :|> xs -> rest :|> (xs :|> x)
    Seq.Empty -> Seq.singleton (Seq.singleton x)
