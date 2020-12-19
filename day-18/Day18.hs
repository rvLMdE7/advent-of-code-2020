{-# LANGUAGE LambdaCase #-}

module Day18 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State (State, get, put, runState)
import Data.ByteString qualified as Byt
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Text.Lazy qualified as Text.Lzy
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec (Parsec, ParsecT)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Chr
import Text.Megaparsec.Char.Lexer qualified as Par.Chr.Lex
import Text.Pretty.Simple (pShowNoColor, pPrint)


type Parser from = Parsec Void from

type StateParser s from to = ParsecT Void from (State s) to

data BinOp = Add | Mult
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Token a
    = TokLit a
    | TokBinOp BinOp
    | TokLParen
    | TokRParen
    deriving (Eq, Ord, Show, Read)

data Expr a
    = ExpLit a
    | ExpBinOp BinOp (Expr a) (Expr a)
    deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
    file <- readFileUtf8 "day-18/input.txt"
    case Par.parse (parseInput <* Par.eof) "day 18 input" file of
        Left err -> putStrLn $ Par.errorBundlePretty err
        Right prog -> do
            pPrint $ part1 prog

part1 :: [[Token Int]] -> Either [String] Int
part1 tokens =
    if null errs
        then Right $ sum $ fmap evalExpr exprs
        else Left errs
  where
    (errs, exprs) = partitionEithers $ fmap getExpr tokens
    runParser = Par.runParserT parseExpr "part 1" .> flip runState Nothing
    getExpr = runParser .> \case
        (Right (), Just expr) -> Right expr
        (Right (), Nothing) -> Left "bad expr"
        (Left err, _mExpr) -> Left $ Text.Lzy.unpack $ pShowNoColor err

parseExpr :: Ord a => StateParser (Maybe (Expr a)) [Token a] ()
parseExpr = void $ Par.some $ Par.choice
    [ parseParenExpr
    , parseLitExpr >>= (Just .> put)
    , parseBinOpSimpleExpr
    ]

parseBinOpSimpleExpr :: Ord a => StateParser (Maybe (Expr a)) [Token a] ()
parseBinOpSimpleExpr = do
    TokBinOp op <- Par.satisfy isBinOp
    mX <- get
    let paren = do
            parseParenExpr
            mY <- get
            case (mX, mY) of
                (Just x, Just y) -> put $ Just $ ExpBinOp op x y
                _ -> fail "bad parenthesized expression"
        literal = do
            y <- parseLitExpr
            case mX of
                Just x -> put $ Just $ ExpBinOp op x y
                Nothing -> fail "bad literal expression"
    paren <|> literal
  where
    isBinOp = \case
        TokBinOp _ -> True
        _tok -> False

parseParenExpr :: Ord a => StateParser (Maybe (Expr a)) [Token a] ()
parseParenExpr =
    Par.between (Par.satisfy isLParen) (Par.satisfy isRParen) parseExpr
  where
    isLParen = \case
        TokLParen -> True
        _tok -> False
    isRParen = \case
        TokRParen -> True
        _tok -> False

parseLitExpr :: Ord a => StateParser s [Token a] (Expr a)
parseLitExpr = do
    TokLit x <- Par.satisfy isLit
    pure $ ExpLit x
  where
    isLit = \case
        TokLit _ -> True
        _tok -> False

evalExpr :: Num a => Expr a -> a
evalExpr = \case
    ExpLit x -> x
    ExpBinOp op x y -> eval op (evalExpr x) (evalExpr y)
  where
    eval = \case
        Add -> (+)
        Mult -> (*)

parseTokens :: Num a => Parser Text [Token a]
parseTokens = Par.some parseToken

parseToken :: Num a => Parser Text (Token a)
parseToken = Par.choice
    [ parseLitToken
    , parseAddToken
    , parseMultToken
    , parseLParenToken
    , parseRParenToken
    ]

parseLitToken :: Num a => Parser Text (Token a)
parseLitToken = lexeme $ TokLit <$> Par.Chr.Lex.decimal

parseAddToken :: Parser Text (Token a)
parseAddToken = lexeme $ TokBinOp Add <$ Par.Chr.char '+'

parseMultToken :: Parser Text (Token a)
parseMultToken = lexeme $ TokBinOp Mult <$ Par.Chr.char '*'

parseLParenToken :: Parser Text (Token a)
parseLParenToken = lexeme $ TokLParen <$ Par.Chr.char '('

parseRParenToken :: Parser Text (Token a)
parseRParenToken = lexeme $ TokRParen <$ Par.Chr.char ')'

lexeme :: Parser Text a -> Parser Text a
lexeme = Par.Chr.Lex.lexeme Par.Chr.hspace

parseInput :: Num a => Parser Text [[Token a]]
parseInput = parseTokens `Par.sepEndBy1` Par.Chr.newline

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path
