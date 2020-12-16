{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Control.Applicative ((<|>), some, many)
import Control.Monad (void)
import Data.ByteString qualified as Byt
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
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

part1 :: Map Text Rule -> [Ticket] -> Int
part1 = sumOfInvalidValsOnTickets

testRange :: Range -> Int -> Bool
testRange (a, b) = between a b

testRule :: Rule -> Int -> Bool
testRule (a, b) x = testRange a x || testRange b x

testRules :: Foldable t => t Rule -> Int -> Bool
testRules rules x = any (`testRule` x) rules

invalidValsOnTicket :: Foldable t => t Rule -> Ticket -> [Int]
invalidValsOnTicket rules = filter (testRules rules .> not)

invalidValsOnTickets :: Foldable t => t Rule -> [Ticket] -> [Int]
invalidValsOnTickets rules = concatMap (invalidValsOnTicket rules)

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
parseRules = Map.fromList <$> Par.sepEndBy1 parseRule Par.Ch.newline

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

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path
