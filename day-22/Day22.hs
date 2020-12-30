{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Optics
    ( AffineTraversal', Optic, Is, A_Setter, (%), (^?), noPrefixFieldLabels
    , makeFieldLabelsWith, _Just, use, at, ix )
import Optics.State.Operators ((?=), (%=))
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex


type Parser a = Parse.Parsec Void Text a

data Player = One | Two
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Decks = Map Player (Seq Int)

data GameState = MkGameState
    { roundNum :: Int
    , playerDecks :: Decks
    , winner :: Maybe Player
    , seenBefore :: Set Decks
    , recurseDepth :: Int
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

part1 :: Decks -> Either String Int
part1 decks = do
    lastState <- runExcept $ execStateT playGame initState
    player <- maybeToEither "no winner" winner lastState
    maybeToEither "no deck" (Map.lookup player) (scoreGame lastState)
  where
    initState = MkGameState
        { roundNum = 0
        , playerDecks = decks
        , winner = Nothing
        , seenBefore = Set.empty
        , recurseDepth = 0
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
                EQ -> throwError "same cards"
        (Just _one, Nothing) -> #winner ?= One
        (Nothing, Just _two) -> #winner ?= Two
        (Nothing, Nothing) -> throwError "both decks empty"

withDeck :: Player -> AffineTraversal' GameState (Seq Int)
withDeck player = #playerDecks % at player % _Just

appendEnd :: a -> a -> Seq a -> Seq a
appendEnd x y list = list :|> x :|> y

playGameRecurse :: Game ()
playGameRecurse = do
    over <- isJust <$> use #winner
    unless over (playRoundRecurse *> playGameRecurse)

playRoundsRecurse :: Int -> Game ()
playRoundsRecurse n = replicateM_ n playRoundRecurse

playRoundRecurse :: Game ()
playRoundRecurse = do
    decks <- use #playerDecks
    before <- use #seenBefore
    depth <- use #recurseDepth

    let deckNum player = if player == One then "one" else "two"
        topCard player = case decks ^? at player % _Just % ix 0 of
            Nothing -> throwError $
                [Printf.s|depth %i: deck %s empty|] depth (deckNum player)
            Just card -> pure card
        deckSize player = case decks Map.!? player of
            Nothing -> throwError $
                [Printf.s|depth %i: missing deck %s|] depth (deckNum player)
            Just deck -> pure $ Seq.length deck

    if decks `Set.member` before
    then #winner ?= One
    else do
        (oneCard, twoCard) <- (,) <$> topCard One <*> topCard Two
        (oneSize, twoSize) <- (,) <$> deckSize One <*> deckSize Two

        #roundNum += 1
        #seenBefore %= Set.insert decks
        withDeck One %= Seq.deleteAt 0
        withDeck Two %= Seq.deleteAt 0

        let mkNewDeck = \case
                One -> Seq.drop 1 .> Seq.take oneCard
                Two -> Seq.drop 1 .> Seq.take twoCard
            initState = MkGameState
                { roundNum = 0
                , playerDecks = Map.mapWithKey mkNewDeck decks
                , winner = Nothing
                , seenBefore = Set.empty
                , recurseDepth = depth + 1
                }
            subGame = runExcept $ execStateT playGameRecurse initState

        (player, won, lost) <-
            if (oneSize > oneCard) && (twoSize > twoCard)
            then case subGame of
                Left err -> throwError err
                Right finalState -> case winner finalState of
                    Just One -> pure (One, oneCard, twoCard)
                    Just Two -> pure (Two, twoCard, oneCard)
                    Nothing -> throwError $
                        [Printf.s|depth %i: no winner|] (depth + 1)
            else case oneCard `compare` twoCard of
                LT -> pure (Two, twoCard, oneCard)
                GT -> pure (One, oneCard, twoCard)
                EQ -> throwError $ [Printf.s|depth %i: same cards|] depth

        withDeck player %= appendEnd won lost

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

parseInput :: Parser Decks
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
