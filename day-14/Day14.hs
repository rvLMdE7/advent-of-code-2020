{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Day14 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State (State, execState)
import Data.Bifunctor (second)
import Data.Bits (Bits, (.|.), (.&.), complement, zeroBits, setBit, clearBit)
import Data.Bool (bool)
import Data.ByteString qualified as B
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (subsequences)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Optics ((%), at, view, noPrefixFieldLabels, makeFieldLabelsWith)
import Optics.State (use)
import Optics.State.Operators ((?=), (.=))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL


type Parser = P.Parsec Void T.Text

data Bit = One | Zero | Cross
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Instr a
    = SetMask [Bit]
    | WriteMem a a
    deriving (Eq, Ord, Show, Read)

data ProgState1 a = MkProgState1
    { memory1 :: M.Map a a
    , bitMask1 :: (a, a)
    } deriving (Eq, Ord, Show, Read)

makeFieldLabelsWith noPrefixFieldLabels ''ProgState1

data ProgState2 a = MkProgState2
    { memory2 :: M.Map a a
    , bitMask2 :: a -> [a]
    }

makeFieldLabelsWith noPrefixFieldLabels ''ProgState2

main :: IO ()
main = do
    file <- readFileUtf8 "day-14/input.txt"
    case P.parse (parseProgram <* P.eof) "day 14 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right prog -> do
            print $ part1 prog
            print $ part2 prog

part1 :: [Instr Int64] -> Int64
part1 = runProgram1 .> view #memory1 .> sum

part2 :: [Instr Int64] -> Int64
part2 = runProgram2 .> view #memory2 .> sum

runProgram1 :: (Integral a, Bits a, Ord a) => [Instr a] -> ProgState1 a
runProgram1 instrs = execState (interpProgram1 instrs) initState
  where
    initState = MkProgState1
        { memory1 = M.empty
        , bitMask1 = (complement zeroBits, zeroBits)
        }

interpProgram1
    :: (Integral a, Bits a, Ord a) => [Instr a] -> State (ProgState1 a) ()
interpProgram1 = foldr (\i comp -> interpInstr1 i *> comp) (pure ())

interpInstr1
    :: (Integral a, Bits a, Ord a) => Instr a -> State (ProgState1 a) ()
interpInstr1 = \case
    SetMask mask -> #bitMask1 .= decode1 mask
    WriteMem address value -> do
        mask <- use #bitMask1
        #memory1 % at address ?= applyMask mask value

runProgram2 :: (Integral a, Bits a, Ord a) => [Instr a] -> ProgState2 a
runProgram2 instrs = execState (interpProgram2 instrs) initState
  where
    initState = MkProgState2
        { memory2 = M.empty
        , bitMask2 = \x -> [x]
        }

interpProgram2
    :: (Integral a, Bits a, Ord a) => [Instr a] -> State (ProgState2 a) ()
interpProgram2 = foldr (\i comp -> interpInstr2 i *> comp) (pure ())

interpInstr2
    :: (Integral a, Bits a, Ord a) => Instr a -> State (ProgState2 a) ()
interpInstr2 = \case
    SetMask mask -> #bitMask2 .= decode2 mask
    WriteMem rawAddr value -> do
        mask <- use #bitMask2
        for_ (mask rawAddr) $ \addr -> #memory2 % at addr ?= value

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

applyMask :: Bits a => (a, a) -> a -> a
applyMask (zeroes, ones) x = (x .&. zeroes) .|. ones

parseProgram :: Integral a => Parser [Instr a]
parseProgram = parseInstr `P.sepEndBy` PC.newline

parseInstr :: Integral a => Parser (Instr a)
parseInstr = parseSetMask <|> parseWriteMem

parseSetMask :: Integral a => Parser (Instr a)
parseSetMask = do
    void $ PC.hspace *> PC.string "mask"
    void $ PC.hspace *> PC.char '='
    rawBits <- PC.hspace *> P.many maskingBit
    pure $ SetMask rawBits
  where
    maskingBit = P.choice
        [ PC.char '0' $> Zero
        , PC.char '1' $> One
        , PC.char 'X' $> Cross
        ]

-- Observe that, given any mask like
--
--     XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
--
-- if we set the values
--
--     maskStr  =  XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
--     zeroMask = (111111111111111111111111111111111101 .&.)
--     oneMask  = (000000000000000000000000000001000000 .|.)
--
-- (where we have included the original `maskStr` for reference), then we can
-- set the desired bits by doing `zeroMask . oneMask`.
--
-- Here `oneMask` has bits set where there is a 1 in the original mask, and all
-- other bits cleared. Meanwhile `zeroMask` has bits cleared where there is
-- a 0 in the original mask, and all other bits set.
--
-- Given all this, our idea is to represent each mask as two integers,
-- representing `zeroMask` and `oneMask` as described here.

decode1 :: forall a. Integral a => [Bit] -> (a, a)
decode1 rawBits = (zeroMask, oneMask)
  where
    indexed = reverse rawBits `zip` enumFrom (0 :: a)
    zeroMask = sum (mkZero <$> indexed)
    oneMask = sum (mkOne <$> indexed)
    mkZero (bits, i) = if bits == Zero then 0 else 2^i
    mkOne (bits, i) = if bits == One then 2^i else 0

decode2 :: forall a. (Integral a, Bits a) => [Bit] -> a -> [a]
decode2 rawBits x = do
    floating <- do
        ixes <- Set.fromList <$> subsequences crossIxes
        pure $ do
            ix <- crossIxes
            pure (ix, ix `Set.member` ixes)
    pure $ assignBits (ones <> floating) x
  where
    bitsIxed = [0 ..] `zip` reverse rawBits
    crossIxes = fst <$> filter (snd .> (==) Cross) bitsIxed
    ones = second (const True) <$> filter (snd .> (==) One) bitsIxed

assignBit :: Bits a => Int -> Bool -> a -> a
assignBit i b x = bool clearBit setBit b x i

assignBits :: Bits a => [(Int, Bool)] -> a -> a
assignBits list x = foldr (uncurry assignBit) x list

parseWriteMem :: Integral a => Parser (Instr a)
parseWriteMem = do
    void $ PC.hspace *> PC.string "mem"
    void $ PC.hspace *> PC.char '['
    address <- PC.hspace *> PCL.decimal
    void $ PC.hspace *> PC.char ']'
    void $ PC.hspace *> PC.char '='
    value <- PC.hspace *> PCL.decimal
    pure $ WriteMem address value
