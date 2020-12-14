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
import Data.Bits (Bits, (.|.), (.&.), complement, zeroBits)
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Map qualified as M
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

data Instr a
    = SetMask a a
    | WriteMem a a
    deriving (Eq, Ord, Show, Read)

data ProgState a = MkProgState
    { memory :: M.Map a a
    , bitMask :: (a, a)
    } deriving (Eq, Ord, Show, Read)

makeFieldLabelsWith noPrefixFieldLabels ''ProgState

main :: IO ()
main = do
    file <- readFileUtf8 "day-14/input.txt"
    case P.parse (parseProgram <* P.eof) "day 14 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right prog -> do
            print $ part1 prog

part1 :: [Instr Int64] -> Int64
part1 = runProgram .> view #memory .> sum

runProgram :: (Bits a, Ord a) => [Instr a] -> ProgState a
runProgram instrs = execState (interpProgram instrs) initState
  where
    initState = MkProgState
        { memory = M.empty
        , bitMask = (complement zeroBits, zeroBits)
        }

interpProgram :: (Bits a, Ord a) => [Instr a] -> State (ProgState a) ()
interpProgram = foldr (\i comp -> interpInstr i *> comp) (pure ())

interpInstr :: (Bits a, Ord a) => Instr a -> State (ProgState a) ()
interpInstr = \case
    SetMask zeroes ones -> #bitMask .= (zeroes, ones)
    WriteMem address value -> do
        mask <- use #bitMask
        #memory % at address ?= applyMask mask value

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

applyMask :: Bits a => (a, a) -> a -> a
applyMask (zeroes, ones) x = (x .&. zeroes) .|. ones

parseProgram :: Integral a => Parser [Instr a]
parseProgram = parseInstr `P.sepEndBy` PC.newline

parseInstr :: Integral a => Parser (Instr a)
parseInstr = parseSetMask <|> parseWriteMem

parseSetMask :: forall a. Integral a => Parser (Instr a)
parseSetMask = do
    void $ PC.hspace *> PC.string "mask"
    void $ PC.hspace *> PC.char '='
    rawBits <- PC.hspace *> P.many maskingBit
    let indexed = reverse rawBits `zip` enumFrom (0 :: a)
        zeroMask = sum (mkZero <$> indexed)
        oneMask = sum (mkOne <$> indexed)
    pure $ SetMask zeroMask oneMask
  where
    maskingBit = P.choice
        [ PC.char '0' $> Zero
        , PC.char '1' $> One
        , PC.char 'X' $> Cross
        ]
    mkZero (bits, i) = if bits == Zero then 0 else 2^i
    mkOne (bits, i) = if bits == One then 2^i else 0

parseWriteMem :: Integral a => Parser (Instr a)
parseWriteMem = do
    void $ PC.hspace *> PC.string "mem"
    void $ PC.hspace *> PC.char '['
    address <- PC.hspace *> PCL.decimal
    void $ PC.hspace *> PC.char ']'
    void $ PC.hspace *> PC.char '='
    value <- PC.hspace *> PCL.decimal
    pure $ WriteMem address value
