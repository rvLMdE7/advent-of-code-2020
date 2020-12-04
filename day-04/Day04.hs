{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Day04 where

import Control.Applicative (Const, (<|>))
import Control.Monad (replicateM, void)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC


type Parser = P.Parsec Void T.Text

type ValidCreds = Creds Maybe
type NorthPoleCreds = Creds (Const Void)
type Passport = Creds Identity

data Creds f = MkCreds
    { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: (Int, Unit)
    , hairColour :: RGB
    , eyeColour :: T.Text
    , passportID :: T.Text
    , countryID :: f T.Text
    }

deriving instance Eq (f T.Text) => Eq (Creds f)
deriving instance Ord (f T.Text) => Ord (Creds f)
deriving instance Show (f T.Text) => Show (Creds f)
deriving instance Read (f T.Text) => Read (Creds f)

data Unit = Cm | In
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data RGB = MkRGB
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving (Bounded, Eq, Ord, Read, Show)

data V3 a = V3 !a !a !a
    deriving (Bounded, Eq, Functor, Foldable, Ord, Read, Show)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-04/input.txt"
    print $ part1 input

part1 :: [M.Map T.Text T.Text] -> Int
part1 = mapMaybe parseCreds .> length

parseCreds :: M.Map T.Text T.Text -> Maybe ValidCreds
parseCreds dict = do
    byr <- P.parseMaybe parseYear =<< M.lookup "byr" dict
    iyr <- P.parseMaybe parseYear =<< M.lookup "iyr" dict
    eyr <- P.parseMaybe parseYear =<< M.lookup "eyr" dict
    hgt <- P.parseMaybe parseHeight =<< M.lookup "hgt" dict
    hcl <- P.parseMaybe parseColour =<< M.lookup "hcl" dict
    ecl <- M.lookup "ecl" dict
    pid <- M.lookup "pid" dict
    let cid = M.lookup "cid" dict
    pure $ MkCreds
        { birthYear = byr
        , issueYear = iyr
        , expirationYear = eyr
        , height = hgt
        , hairColour = hcl
        , eyeColour = ecl
        , passportID = pid
        , countryID = cid
        }

parseYear :: Parser Int
parseYear = read <$> P.some PC.digitChar

parseHeight :: Parser (Int, Unit)
parseHeight = do
    hgt <- read <$> P.some PC.digitChar
    unit <- (PC.string "cm" $> Cm) <|> (PC.string "in" $> In)
    pure (hgt, unit)

parseColour :: Parser RGB
parseColour = do
    void $ PC.char '#'
    let readHex cs = read ("0x" <> cs)
    r <- readHex <$> replicateM 2 PC.hexDigitChar
    g <- readHex <$> replicateM 2 PC.hexDigitChar
    b <- readHex <$> replicateM 2 PC.hexDigitChar
    pure $ MkRGB r g b

parseInput :: T.Text -> [M.Map T.Text T.Text]
parseInput txt =
  let
    mkKeyVal = T.breakOn ":" .> second (T.drop 1)
  in
    fmap (T.words .> fmap mkKeyVal .> M.fromList) (T.splitOn "\n\n" txt)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
