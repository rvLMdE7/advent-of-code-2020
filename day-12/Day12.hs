{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day12 where

import Control.Monad.State (MonadState, State, execState)
import Data.ByteString qualified as B
import Data.Complex (Complex((:+)), cis)
import Data.Functor (($>))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Optics
    ( Optic, A_Setter, Is, view, noPrefixFieldLabels, makeFieldLabelsWith )
import Optics.State (use)
import Optics.State.Operators ((%=))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC
import Text.Megaparsec.Char.Lexer qualified as PCL
-- import Text.Pretty.Simple (pPrint)


type Parser = P.Parsec Void T.Text

data NavAction
    = MoveNorth
    | MoveSouth
    | MoveEast
    | MoveWest
    | TurnLeft
    | TurnRight
    | MoveForward
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data NavInstr = MkNavInstr
    { navAction :: !NavAction
    , navValue :: !Int
    } deriving (Eq, Ord, Read, Show)

data ShipState = MkShipState
    { shipPhase :: !Double
    , shipPosition :: !(Complex Double)
    } deriving (Eq, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''ShipState

main :: IO ()
main = do
    file <- readFileUtf8 "day-12/input.txt"
    case P.parse (parseNavInstrs <* P.eof) "day 12 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right navInstrs -> do
            print $ part1 navInstrs

part1 :: [NavInstr] -> Double
part1 = pilotShip .> view #shipPosition .> manhattanNorm

pilotShip :: [NavInstr] -> ShipState
pilotShip instrs = execState (applyInstrs instrs) $
    MkShipState
        { shipPhase = 0
        , shipPosition = pure 0
        }

applyInstrs :: [NavInstr] -> State ShipState ()
applyInstrs = foldr (\instr st -> applyInstr instr *> st) (pure ())

applyInstr :: NavInstr -> State ShipState ()
applyInstr (MkNavInstr action intVal) = case action of
    MoveNorth -> #shipPosition += (0 :+ val)
    MoveSouth -> #shipPosition -= (0 :+ val)
    MoveEast -> #shipPosition += (val :+ 0)
    MoveWest -> #shipPosition -= (val :+ 0)
    TurnLeft -> #shipPhase %= (+ val) .> (`modulo` 360)
    TurnRight -> #shipPhase %= subtract val .> (`modulo` 360)
    MoveForward -> do
        phase <- use #shipPhase
        #shipPosition += fmap (* val) (cisDegrees phase)
  where
    val = fromIntegral intVal

cisDegrees :: Double -> Complex Double
cisDegrees deg
    | deg == 0 = 1 :+ 0
    | deg == 90 = 0 :+ 1
    | deg == 180 = (-1) :+ 0
    | deg == 270 = 0 :+ (-1)
    | otherwise = cis rad
  where
    rad = deg * pi / 180

modulo :: Double -> Double -> Double
modulo x m = x - floorDbl (x / m) * m
  where
    floorDbl = floor .> fromIntegral @Int

manhattanDist :: RealFloat a => Complex a -> Complex a -> a
manhattanDist u v = sum $ fmap abs $ v - u

manhattanNorm :: RealFloat a => Complex a -> a
manhattanNorm = manhattanDist (pure 0)

parseNavInstrs :: Parser [NavInstr]
parseNavInstrs = parseNavInstr `P.sepEndBy` PC.space

parseNavInstr :: Parser NavInstr
parseNavInstr = do
    action <- parseNavAction
    val <- PC.space *> PCL.decimal
    pure $ MkNavInstr action val

parseNavAction :: Parser NavAction
parseNavAction = P.choice
    [ PC.char 'N' $> MoveNorth
    , PC.char 'S' $> MoveSouth
    , PC.char 'E' $> MoveEast
    , PC.char 'W' $> MoveWest
    , PC.char 'L' $> TurnLeft
    , PC.char 'R' $> TurnRight
    , PC.char 'F' $> MoveForward
    ]

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

(+=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic += x = optic %= (+ x)

(-=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic -= x = optic %= subtract x
