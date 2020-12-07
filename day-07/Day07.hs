{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Algebra.Graph.Labelled qualified as GL
import Algebra.Graph.ToGraph qualified as G
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.Function ((&))
import Data.Functor (($>))
import Data.Monoid (Sum(Sum))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL


type Parser = P.Parsec Void T.Text

type Bag = T.Text

data Rule a = MkRule
    { outerRule :: a
    , innerRules :: [(a, Int)]
    } deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
    file <- readFileUtf8 "day-07/input.txt"
    case P.parse (parseRules <* P.eof) "day 7 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            let graph = buildGraph input
            print $ part1 graph
            print $ part2 graph

part1 :: GL.Graph (Sum Int) Bag -> Int
part1 graph = S.size $ canContain "shiny gold" graph

part2 :: GL.Graph (Sum Int) Bag -> Int
part2 graph = mustContain "shiny gold" graph

mustContain :: Bag -> GL.Graph (Sum Int) Bag -> Int
mustContain bag graph = go bag - 1
  where
    go :: Bag -> Int
    go u =
      let
        next = G.postSet u graph
        contrib v = let Sum count = GL.edgeLabel u v graph in count * go v
      in
        1 + sumMap contrib next

sumMap :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumMap f = foldr (\x total -> total + f x) 0

canContain :: Bag -> GL.Graph (Sum Int) Bag -> S.Set Bag
canContain bag graph =
    reachable & if nonTrivLoopExists then id else S.delete bag
  where
    reachable = S.fromList $ G.reachable bag (GL.transpose graph)
    nonTrivLoopExists = any nonTrivLoop (G.postSet bag graph)
    nonTrivLoop v = bag `elem` G.reachable v graph

buildGraph :: [Rule Bag] -> GL.Graph (Sum Int) Bag
buildGraph rules = GL.edges $ do
    MkRule outer inners <- rules
    (inner, n) <- inners
    pure (Sum n, outer, inner)

parseRules :: Parser [Rule Bag]
parseRules = parseRule `P.sepEndBy` PC.newline

parseRule :: Parser (Rule Bag)
parseRule = do
    outer <- PC.space *> P.manyTill P.anySingle (PC.string "bags")
    void $ PC.space1 *> PC.string "contain"
    inner <- parseInnerBags
    void $ PC.space *> PC.char '.'
    pure $ MkRule (T.stripEnd $ T.pack outer) inner

parseInnerBags :: Parser [(Bag, Int)]
parseInnerBags = PC.space *> (noBags <|> innerBags)
  where
    noBags = PC.string "no other bags" $> []
    innerBags = parseInnerBag `P.sepBy1` (PC.space *> PC.char ',')

parseInnerBag :: Parser (Bag, Int)
parseInnerBag = do
    n <- PC.space *> PCL.decimal
    col <- PC.space1 *> P.manyTill P.anySingle bags
    pure (T.stripEnd $ T.pack col, n)
  where
    bags = PC.string "bag" *> P.optional (PC.char 's')

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
