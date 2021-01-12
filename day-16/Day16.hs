{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Control.Applicative ((<|>), some, many)
import Control.Monad (void, unless)
import Control.Monad.State (State, gets, execState)
import Data.ByteString qualified as Byt
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Tuple (swap)
import Data.Void (Void)
import Flow ((<.), (.>))
import Optics ((&), (%~), _1, _2)
import Optics.State.Operators ((%=))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex


type Parser a = Parsec Void Text a

type Ticket = [Int]

type Range = (Int, Int)

type Rule = (Range, Range)

main :: IO ()
main = do
    file <- readFileUtf8 "day-16/input.txt"
    case Par.parse (parseInput <* Par.eof) "day 16 input" file of
        Left err -> putStrLn $ Par.errorBundlePretty err
        Right (rules, your, tickets) -> do
            print $ part1 rules tickets
            let validTickets = filter (not <. invalidTicket rules) tickets
            print $ determineFields rules validTickets

part1 :: Map Text Rule -> [Ticket] -> Int
part1 = sumOfInvalidValsOnTickets

determineFields :: Map Text Rule -> [Ticket] -> Map Text Int
determineFields initRules initTickets =
    (fmap Left initRules, ticketsToFields initTickets)
        & execState determine
        & fst
        & Map.mapMaybe rightToMaybe
  where
    determine :: State (Map Text (Either Rule Int), Map Int [Int]) ()
    determine = do
        rules <- gets $ fst .> Map.mapMaybe leftToMaybe
        unless (Map.null rules) $ do
            result <- gets $ snd .> fixedFields rules
            let update = Map.fromList $ fmap (_2 %~ Right) result
            _1 %= Map.union update
            _2 %= deleteKeys (snd <$> result)
            determine

ticketsToFields :: [Ticket] -> Map Int [Int]
ticketsToFields = List.transpose .> zip [0..] .> Map.fromList

fixedFields :: Map Text Rule -> Map Int [Int] -> [(Text, Int)]
fixedFields rules fields = evalRules rules fields
    & fmap (Maybe.mapMaybe $ uncurry maybeBool)
    & Map.mapMaybe singleton
    & Map.toList
    & fmap swap

evalRules :: Map Text Rule -> Map Int [Int] -> Map Int [(Text, Bool)]
evalRules rules = fmap eval
  where
    eval vals = Map.toList $ fmap (test vals) rules
    test vals rule = all (testRule rule) vals

testRange :: Range -> Int -> Bool
testRange (a, b) = between a b

testRule :: Rule -> Int -> Bool
testRule (a, b) x = testRange a x || testRange b x

testRules :: Foldable t => t Rule -> Int -> Bool
testRules rules x = any (`testRule` x) rules

invalidTicket :: Foldable t => t Rule -> Ticket -> Bool
invalidTicket rules = any (testRules rules .> not)

invalidValsOnTicket :: Foldable t => t Rule -> Ticket -> [Int]
invalidValsOnTicket rules = filter $ testRules rules .> not

invalidValsOnTickets :: Foldable t => t Rule -> [Ticket] -> [Int]
invalidValsOnTickets = invalidValsOnTicket .> concatMap

sumOfInvalidValsOnTickets :: Foldable t => t Rule -> [Ticket] -> Int
sumOfInvalidValsOnTickets rules = invalidValsOnTickets rules .> sum

parseInput :: Parser (Map Text Rule, Ticket, [Ticket])
parseInput = do
    rules <- parseRules
    void $ newlines *> Par.Ch.string "your ticket:"
    your <- newlines *> parseTicket
    void $ newlines *> Par.Ch.string "nearby tickets:"
    tickets <- newlines *> parseTickets
    pure (rules, your, tickets)
  where
    newlines = many Par.Ch.newline

parseRules :: Parser (Map Text Rule)
parseRules = Map.fromList <$> parseRule `Par.sepEndBy1` Par.Ch.newline

parseTickets :: Parser [Ticket]
parseTickets = parseTicket `Par.sepEndBy1` Par.Ch.newline

parseTicket :: Parser Ticket
parseTicket = do
    Par.Ch.hspace
    Par.Ch.Lex.decimal `Par.sepBy1` (Par.Ch.hspace *> Par.Ch.char ',')

parseRule :: Parser (Text, Rule)
parseRule = do
    name <- Par.Ch.hspace *> some nameChar
    void $ Par.Ch.hspace *> Par.Ch.char ':'
    a <- parseRange
    void $ Par.Ch.hspace *> Par.Ch.string "or"
    b <- parseRange
    pure (Text.pack name, (a, b))
  where
    nameChar = Par.Ch.letterChar <|> Par.Ch.char ' '

parseRange :: Parser Range
parseRange = do
    a <- Par.Ch.hspace *> Par.Ch.Lex.decimal
    void $ Par.Ch.hspace *> Par.Ch.char '-'
    b <- Par.Ch.hspace *> Par.Ch.Lex.decimal
    pure (a, b)

between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)

maybeBool :: a -> Bool -> Maybe a
maybeBool x cond = if cond then Just x else Nothing

singleton :: [a] -> Maybe a
singleton = \case
    [x] -> Just x
    _ -> Nothing

deleteKeys :: Ord k => [k] -> Map k v -> Map k v
deleteKeys keys dict = foldr Map.delete dict keys

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path
