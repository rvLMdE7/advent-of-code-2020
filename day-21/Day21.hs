{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day21 where

import Control.Monad (void)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Bifunctor (second)
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Optics ((^?), (%~), (%), (&), isn't, mapped)
import Optics qualified
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Pretty.Simple (pPrint)


type Parser = Parse.Parsec Void Text

newtype Ingr = MkIngr
    { unIngr :: Text
    } deriving (Eq, Ord, Read, Show)

newtype Allergen = MkAllergen
    { unAllergen :: Text
    } deriving (Eq, Ord, Read, Show)

type FactDB = Map Allergen Fact

data Fact
    = OneOf (Set Ingr)
    | Equals Ingr
    deriving (Eq, Ord, Read, Show)

Optics.makePrismLabels ''Fact


main :: IO ()
main = do
    file <- readFileUtf8 "day-21/input.txt"
    case Parse.parse (parseInput <* Parse.eof) "day 21 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right input -> do
            pPrint $ part1 input

part1 :: [(Set Allergen, Set Ingr)] -> Int
part1 assocs = sum $ do
    ingr <- Set.toList clears
    pure $ length $ filter (Set.member ingr) ingrs
  where
    ingrs = snd <$> assocs
    knowns = reduce $ compileInput assocs
    clears = Set.unions ingrs Set.\\ elemSet knowns


findFixeds :: Map Allergen Fact -> Map Allergen Ingr
findFixeds = Map.mapMaybe $ \case
    OneOf ingrs | Set.size ingrs == 1 -> Just $ Set.findMin ingrs
    _ -> Nothing

applyFixeds :: Map Allergen Ingr -> State (Map Allergen Fact) ()
applyFixeds fixeds = modify $ \facts -> facts
    & Map.union (Equals <$> fixeds)
    & mapped % #_OneOf %~ (Set.\\ elemSet fixeds)

simplify :: State (Map Allergen Fact) ()
simplify = gets findFixeds >>= applyFixeds

stuck :: State (Map Allergen Fact) Bool
stuck = gets $ all $ isn't #_OneOf

reduce :: Map Allergen Fact -> Map Allergen Ingr
reduce = evalState go
  where
    go = stuck >>= \case
        True -> gets $ Map.mapMaybe (^? #_Equals)
        False -> simplify *> go


parseInput :: Parser [(Set Allergen, Set Ingr)]
parseInput = parseFood `Parse.sepEndBy1` Parse.Char.newline

parseFood :: Parser (Set Allergen, Set Ingr)
parseFood = flip (,) <$> parseIngrs <*> parseAllergens

parseIngrs :: Parser (Set Ingr)
parseIngrs = Set.fromList <$> parseIngr `Parse.sepEndBy` Parse.Char.hspace

parseIngr :: Parser Ingr
parseIngr = mkIngr <$> Parse.some Parse.Char.letterChar
  where
    mkIngr = Text.pack .> MkIngr

parseAllergens :: Parser (Set Allergen)
parseAllergens = Parse.between lParen rParen $ do
    void $ Parse.Char.string "contains" <* Parse.Char.hspace
    Set.fromList <$> parseAllergen `Parse.sepBy` comma
  where
    lParen = Parse.Char.char '('
    rParen = Parse.Char.char ')'
    comma = Parse.Char.char ',' <* Parse.Char.hspace

parseAllergen :: Parser Allergen
parseAllergen = mkAllergen <$> Parse.some Parse.Char.letterChar
  where
    mkAllergen = Text.pack .> MkAllergen


compileInput :: [(Set Allergen, Set Ingr)] -> Map Allergen Fact
compileInput = reogranize .> fmap OneOf

reogranize :: (Ord k, Ord v) => [(Set k, Set v)] -> Map k (Set v)
reogranize = expandAssocs .> combineAssocs .> flattenMap

expandAssocs :: [(Set k, Set v)] -> [(k, Set v)]
expandAssocs assocs = do
    (allergens, ingrs) <- assocs
    allergen <- Set.toList allergens
    pure (allergen, ingrs)

combineAssocs :: Ord k => [(k, v)] -> Map k (NonEmpty v)
combineAssocs = fmap (second singleton) .> Map.fromListWith (<>)

flattenMap :: Ord v => Map k (NonEmpty (Set v)) -> Map k (Set v)
flattenMap = fmap intersections


intersections :: Ord a => NonEmpty (Set a) -> Set a
intersections (set :| sets) = foldr Set.intersection set sets

elemSet :: Ord v => Map k v -> Set v
elemSet = Map.elems .> Set.fromList

singleton :: a -> NonEmpty a
singleton x = x :| []


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
