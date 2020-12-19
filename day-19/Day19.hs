{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.ByteString qualified as Bytes
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex
import Text.Pretty.Simple (pPrint)


type Parser = Parsec Void Text

data Rule = SingleChar Char | SubRules [NonEmpty Int]
    deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
    file <- readFileUtf8 "day-19/input.txt"
    case Parse.parse (parseInput <* Parse.eof) "day 19 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right (rules, msgs) -> do
            pPrint $ part1 rules msgs
            pPrint $ part2 rules msgs

part1 :: IntMap Rule -> [Text] -> Int
part1 rules = matches (compile rules 0) .> length

part2 :: IntMap Rule -> [Text] -> Int
part2 rules = matches (compile rules' 0) .> length
  where
    rules' = rules
        & IntMap.insert 8
            ( SubRules
                [ 42 :| []
                , 42 :| [8]
                ]
            )
        & IntMap.insert 11
            ( SubRules
                [ 42 :| [31]
                , 42 :| [11, 31]
                ]
            )

matches :: Parser () -> [Text] -> [Text]
matches parser = filter (match parser)

match :: Parser () -> Text -> Bool
match parser = Parse.parse (parser <* Parse.eof) "match" .> isRight

compile :: IntMap Rule -> Int -> Parser ()
compile rules n = case rules IntMap.!? n of
    Nothing -> fail $ [Printf.s|rule %i not present|] n
    Just (SingleChar c) -> void $ Parse.Char.char c
    Just (SubRules subRules) -> Parse.choice $ do
        first :| rest <- subRules
        pure $ Parse.try $ foldr chain (compile rules first) rest
  where
    chain i parser = parser *> compile rules i

parseInput :: Parser (IntMap Rule, [Text])
parseInput = do
    rules <- parseRules
    void $ Parse.many Parse.Char.newline
    msgs <- parseMessages
    pure (rules, msgs)

parseRules :: Parser (IntMap Rule)
parseRules = IntMap.fromList <$> parseRule `Parse.sepEndBy1` Parse.Char.newline

parseRule :: Parser (Int, Rule)
parseRule = do
    n <- Parse.Char.Lex.decimal <* Parse.Char.hspace
    void $ Parse.Char.char ':' <* Parse.Char.hspace
    rule <- parseRuleSingleChar <|> parseRuleSubRules
    pure (n, rule)

parseRuleSingleChar :: Parser Rule
parseRuleSingleChar = SingleChar <$> (anyChar <* Parse.Char.hspace)
  where
    anyChar = Parse.between quote quote (Parse.satisfy $ isSpace .> not)
    quote = Parse.Char.char '"'

parseRuleSubRules :: Parser Rule
parseRuleSubRules = SubRules <$> subRule `Parse.sepBy` pipe
  where
    pipe = Parse.Char.char '|' <* Parse.Char.hspace
    subRule = NonEmpty.fromList <$>
        Parse.Char.Lex.decimal `Parse.sepEndBy1` Parse.Char.hspace

parseMessages :: Parser [Text]
parseMessages =
    (parseMessage <* Parse.Char.hspace) `Parse.sepEndBy1` Parse.Char.newline

parseMessage :: Parser Text
parseMessage = Text.pack <$> Parse.some (Parse.satisfy $ isSpace .> not)

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
