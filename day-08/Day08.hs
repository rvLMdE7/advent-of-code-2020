{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Set qualified as S
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL
import Data.Functor (($>))
import Control.Monad.State ( MonadState, runState, State )
import Optics
    ( Optic, Is, A_Setter, (%), (^.), preuse, use, makeLenses, at, ix )
import Optics.State.Operators ((%=), (?=))


type Parser = P.Parsec Void T.Text

data Operation = Acc | Jmp | Nop
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Instr = MkInstr
    { _operation :: Operation
    , _inputVal :: Int
    } deriving (Bounded, Eq, Ord, Show, Read)

makeLenses ''Instr

type Program = V.Vector Instr

data ProgramState = MkProgramState
    { _instrPtr :: Int
    , _accumulator :: Int
    , _program :: Program
    , _visited :: S.Set Int
    } deriving (Eq, Ord, Read, Show)

makeLenses ''ProgramState

data Result = Looped | Stopped
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

main :: IO ()
main = do
    file <- readFileUtf8 "day-08/input.txt"
    case P.parse (parseProgram <* P.eof) "day 8 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            print $ part1 input

part1 :: Program -> Maybe Int
part1 prog = if result == Looped
    then Just (progState ^. accumulator)
    else Nothing
  where
    (result, progState) = runProgram prog

runProgram :: Program -> (Result, ProgramState)
runProgram prog = runState interpProgramUntilLoopOrStop $
    MkProgramState
        { _instrPtr = 0
        , _accumulator = 0
        , _program = prog
        , _visited = S.empty
        }

interpProgramUntilLoopOrStop :: State ProgramState Result
interpProgramUntilLoopOrStop = do
    ptr <- use instrPtr
    vis <- use visited
    if ptr `S.member` vis
        then pure Looped
        else do
            visited % at ptr ?= ()
            mayInstr <- preuse (program % ix ptr)
            case mayInstr of
                Just instr -> do
                    applyInstr instr
                    interpProgramUntilLoopOrStop
                Nothing -> pure Stopped

applyInstr :: Instr -> State ProgramState ()
applyInstr (MkInstr op n) = case op of
    Nop -> instrPtr += 1
    Jmp -> instrPtr += n
    Acc -> do
        accumulator += n
        instrPtr += 1

parseProgram :: Parser Program
parseProgram = V.fromList <$> P.sepEndBy1 parseInstr PC.newline

parseInstr :: Parser Instr
parseInstr = MkInstr <$> parseOperation <*> parseInput

parseOperation :: Parser Operation
parseOperation = PC.space *> P.choice
    [ PC.string "acc" $> Acc
    , PC.string "jmp" $> Jmp
    , PC.string "nop" $> Nop
    ]

parseInput :: Parser Int
parseInput = PC.space *> PCL.signed PC.space PCL.decimal

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

(+=)
    :: (Is k A_Setter, MonadState s m, Num b)
    => Optic k is s s b b
    -> b
    -> m ()
optic += x = optic %= (+ x)
