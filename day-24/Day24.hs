{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import Data.Bifunctor (bimap)
import Data.ByteString qualified as Bytes
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char


type Parser = Parse.Parsec Void Text

data HexDir = E | SE | SW | W | NW | NE
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


main :: IO ()
main = do
    file <- readFileUtf8 "day-24/input.txt"
    case Parse.parse (parseHexPaths <* Parse.eof) "day 24 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right paths -> do
            print $ part1 paths

part1 :: [[HexDir]] -> Int
part1 = fmap canonPath .> count .> Map.filter odd .> length


canonPath :: [HexDir] -> [HexDir]
canonPath = canon (loops <> shortcuts)

loops :: Map (Set HexDir) (Set HexDir)
loops = Map.fromList $
    bimap Set.fromList Set.fromList <$>
        [ ([E, W], [])
        , ([SE, NW], [])
        , ([NE, SW], [])
        ]

shortcuts :: Map (Set HexDir) (Set HexDir)
shortcuts = Map.fromList $
    bimap Set.fromList Set.fromList <$>
        [ ([W, NE], [NW])
        , ([W, SE], [SW])
        , ([NE, SE], [E])
        , ([E, NW], [NE])
        , ([E, SW], [SE])
        , ([NW, SW], [W])
        ]


canon :: Ord a => Map (Set a) (Set a) -> [a] -> [a]
canon rules = count .> applyRules rules .> unCount

applyRules :: Ord a => Map (Set a) (Set a) -> Map a Int -> Map a Int
applyRules rules = apply
  where
    apply dict = if any (`applicable` dict) (Map.keysSet rules)
        then apply $ Map.foldrWithKey (curry applyRule) dict rules
        else dict

applyRule :: Ord a => (Set a, Set a) -> Map a Int -> Map a Int
applyRule (from, to) dict
    | occ /= 0 = dict
        & Map.mapMaybeWithKey decr
        & Map.unionWith (+) incr
    | otherwise = dict
  where
    occ = if Set.null from
        then 0
        else minimum $ Set.map (\f -> Map.findWithDefault 0 f dict) from
    decr key n = if key `Set.member` from
        then if occ == n
            then Nothing
            else Just $ n - occ
        else Just n
    incr = Map.fromSet (const occ) to

applicable :: Ord a => Set a -> Map a Int -> Bool
applicable from dict = all (`Map.member` dict) from


count :: Ord a => [a] -> Map a Int
count = foldr insert Map.empty
  where
    insert x = Map.insertWith (+) x 1

unCount :: Map a Int -> [a]
unCount = Map.toAscList .> concatMap repl
  where
    repl (a, n) = replicate n a


parseHexPaths :: Parser [[HexDir]]
parseHexPaths = parseHexDirs `Parse.sepEndBy` Parse.Char.newline

parseHexDirs :: Parser [HexDir]
parseHexDirs = parseHexDir `Parse.sepEndBy1` Parse.Char.hspace

parseHexDir :: Parser HexDir
parseHexDir = Parse.choice
    [ Parse.Char.string "e" $> E
    , Parse.Char.string "se" $> SE
    , Parse.Char.string "sw" $> SW
    , Parse.Char.string "w" $> W
    , Parse.Char.string "nw" $> NW
    , Parse.Char.string "ne" $> NE
    ]


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
