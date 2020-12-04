{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Day04 where

import Control.Applicative (Const)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
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
    , height :: T.Text
    , hairColour :: T.Text
    , eyeColour :: T.Text
    , passportID :: T.Text
    , countryID :: f T.Text
    }

deriving instance Eq (f T.Text) => Eq (Creds f)
deriving instance Ord (f T.Text) => Ord (Creds f)
deriving instance Show (f T.Text) => Show (Creds f)
deriving instance Read (f T.Text) => Read (Creds f)

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
    hgt <- M.lookup "hgt" dict
    hcl <- M.lookup "hcl" dict
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

parseInput :: T.Text -> [M.Map T.Text T.Text]
parseInput txt =
  let
    mkKeyVal = T.breakOn ":" .> second (T.drop 1)
  in
    fmap (T.words .> fmap mkKeyVal .> M.fromList) (T.splitOn "\n\n" txt)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
