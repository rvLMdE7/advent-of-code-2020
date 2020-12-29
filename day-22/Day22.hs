{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day22 where

import Control.Monad (replicateM_, unless, void)
import Control.Monad.Except (Except, throwError, runExcept)
import Control.Monad.State (MonadState, StateT, execStateT)
import Data.ByteString qualified as Bytes
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq((:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Optics
    ( Optic, Is, A_Setter, (%), (^?), noPrefixFieldLabels, makeFieldLabelsWith
    , _Just, use, at, ix )
import Optics.State.Operators ((?=), (%=))
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex


type Parser a = Parse.Parsec Void Text a

data Player = One | Two
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data GameState = MkGameState
    { roundNum :: Int
    , playerDecks :: Map Player (Seq Int)
    , winner :: Maybe Player
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''GameState

type Game a = StateT GameState (Except String) a

main :: IO ()
main = do
    file <- readFileUtf8 "day-22/input.txt"
    case Parse.parse (parseInput <* Parse.eof) "day 22 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right decks -> do
            print $ part1 decks

part1 :: Map Player (Seq Int) -> Either String Int
part1 decks = do
    lastState <- runExcept $ execStateT playGame initState
    player <- maybeToEither "no winner" winner lastState
    maybeToEither "no deck" (Map.lookup player) (scoreGame lastState)
  where
    initState = MkGameState
        { roundNum = 0
        , playerDecks = decks
        , winner = Nothing
        }

scoreGame :: GameState -> Map Player Int
scoreGame = playerDecks .> fmap scoreDeck

scoreDeck :: Seq Int -> Int
scoreDeck deck = sum $ Seq.zipWith (*) deck decr
  where
    len = Seq.length deck
    decr = Seq.fromFunction len (len -)

playGame :: Game ()
playGame = do
    over <- isJust <$> use #winner
    unless over (playRound *> playGame)

playRounds :: Int -> Game ()
playRounds n = replicateM_ n playRound

playRound :: Game ()
playRound = do
    decks <- use #playerDecks
    let topCard player = decks ^? at player % _Just % ix 0
    case (topCard One, topCard Two) of
        (Just one, Just two) -> do
            #roundNum += 1
            withDeck One %= Seq.deleteAt 0
            withDeck Two %= Seq.deleteAt 0
            case one `compare` two of
                LT -> withDeck Two %= appendEnd two one
                GT -> withDeck One %= appendEnd one two
                EQ -> throwError "playRound: same cards on top"
        (Just _one, Nothing) -> #winner ?= One
        (Nothing, Just _two) -> #winner ?= Two
        (Nothing, Nothing) -> throwError "playRound: both decks empty"
  where
    withDeck player = #playerDecks % at player % _Just
    appendEnd x y deck = deck :|> x :|> y

(+=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic += x = optic %= (+ x)

maybeToEither :: b -> (a -> Maybe c) -> a -> Either b c
maybeToEither left mRight x = case mRight x of
    Nothing -> Left left
    Just right -> Right right

parseInput :: Parser (Map Player (Seq Int))
parseInput = do
    decks <- parsePlayerDeck `Parse.sepEndBy` Parse.Char.newline
    pure $ Map.fromList decks

parsePlayerDeck :: Parser (Player, Seq Int)
parsePlayerDeck = do
    player <- parsePlayer
    void Parse.Char.newline
    deck <- parseDeck
    pure (player, deck)

parsePlayer :: Parser Player
parsePlayer = do
    void $ Parse.Char.string "Player" <* Parse.Char.hspace
    player <- Parse.choice
        [ Parse.Char.char '1' $> One
        , Parse.Char.char '2' $> Two
        ]
    void $ Parse.Char.hspace *> Parse.Char.char ':' <* Parse.Char.hspace
    pure player

parseDeck :: Parser (Seq Int)
parseDeck = do
    cards <- Parse.Char.Lex.decimal `Parse.sepEndBy` Parse.Char.newline
    pure $ Seq.fromList cards

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
