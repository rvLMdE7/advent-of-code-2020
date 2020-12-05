{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Control.Applicative ((<|>), many)
import Control.Monad (void, guard, replicateM)
import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC


type Parser = P.Parsec Void T.Text

data Creds = MkCreds
    { crBirthYear :: Int
    , crIssueYear :: Int
    , crExpirationYear :: Int
    , crHeight :: T.Text
    , crHairColour :: T.Text
    , crEyeColour :: T.Text
    , crPassportID :: T.Text
    , crCountryID :: Maybe T.Text
    } deriving (Eq, Ord, Show, Read)

data StrictCreds = MkStrictCreds
    { stBirthYear :: Int
    , stIssueYear :: Int
    , stExpirationYear :: Int
    , stHeight :: Height
    , stHairColour :: RGB
    , stEyeColour :: EyeColour
    , stPassportID :: Int
    , stCountryID :: Maybe T.Text
    } deriving (Eq, Ord, Show, Read)

data RGB = MkRGB
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving (Bounded, Eq, Ord, Show, Read)

data Height = MkHeight
    { hgtAmount :: Int
    , hgtUnit :: Unit
    } deriving (Bounded, Eq, Ord, Show, Read)

data Unit = Cm | In
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data EyeColour = Amber | Blue | Brown | Grey | Green | Hazel | Other
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-04/input.txt"
    print $ part1 input
    print $ part2 input

part1 :: [M.Map T.Text T.Text] -> Int
part1 = mapMaybe parseCreds .> length

part2 :: [M.Map T.Text T.Text] -> Int
part2 = mapMaybe parseCredsStrict .> length

parseCreds :: M.Map T.Text T.Text -> Maybe Creds
parseCreds dict = do
    byr <- M.lookup "byr" dict >>= P.parseMaybe parseYear
    iyr <- M.lookup "iyr" dict >>= P.parseMaybe parseYear
    eyr <- M.lookup "eyr" dict >>= P.parseMaybe parseYear
    hgt <- M.lookup "hgt" dict
    hcl <- M.lookup "hcl" dict
    ecl <- M.lookup "ecl" dict
    pid <- M.lookup "pid" dict
    let cid = M.lookup "cid" dict
    pure $ MkCreds
        { crBirthYear = byr
        , crIssueYear = iyr
        , crExpirationYear = eyr
        , crHeight = hgt
        , crHairColour = hcl
        , crEyeColour = ecl
        , crPassportID = pid
        , crCountryID = cid
        }

parseYear :: Parser Int
parseYear = read <$> P.some PC.digitChar

parseCredsStrict :: M.Map T.Text T.Text -> Maybe StrictCreds
parseCredsStrict dict = do
    byr <- M.lookup "byr" dict >>= P.parseMaybe (year 1920 2002)
    iyr <- M.lookup "iyr" dict >>= P.parseMaybe (year 2010 2020)
    eyr <- M.lookup "eyr" dict >>= P.parseMaybe (year 2020 2030)
    hgt <- M.lookup "hgt" dict >>= P.parseMaybe height
    hcl <- M.lookup "hcl" dict >>= P.parseMaybe parseRGBColStrict
    ecl <- M.lookup "ecl" dict >>= P.parseMaybe parseEyeColStrict
    pid <- M.lookup "pid" dict >>= P.parseMaybe parsePassIDStrict
    let cid = M.lookup "cid" dict
    pure $ MkStrictCreds
        { stBirthYear = byr
        , stIssueYear = iyr
        , stExpirationYear = eyr
        , stHeight = hgt
        , stHairColour = hcl
        , stEyeColour = ecl
        , stPassportID = pid
        , stCountryID = cid
        }
  where
    year a b = parseYearStrict $ between a b
    height = parseHeightStrict $ \(MkHeight amt unit) -> case unit of
        Cm -> between 150 193 amt
        In -> between 59 76 amt

parseYearStrict :: (Int -> Bool) -> Parser Int
parseYearStrict f = do
    year <- read <$> replicateM 4 PC.digitChar
    P.eof
    guard $ f year
    pure year

parseHeightStrict :: (Height -> Bool) -> Parser Height
parseHeightStrict f = do
    amt <- read <$> many PC.digitChar
    unit <- (PC.string "cm" $> Cm) <|> (PC.string "in" $> In)
    P.eof
    let height = MkHeight amt unit
    guard $ f height
    pure height

parseRGBColStrict :: Parser RGB
parseRGBColStrict = do
    void $ PC.char '#'
    let readCol cs = read ("0x" <> cs)
        legalChars = "0123456789abcdef" :: [Char]
        component = readCol <$> replicateM 2 (P.oneOf legalChars)
    rgb <- MkRGB <$> component <*> component <*> component
    P.eof
    pure rgb

parseEyeColStrict :: Parser EyeColour
parseEyeColStrict = do
    col <- P.choice
        [ PC.string "amb" $> Amber
        , PC.string "blu" $> Blue
        , PC.string "brn" $> Brown
        , PC.string "gry" $> Grey
        , PC.string "grn" $> Green
        , PC.string "hzl" $> Hazel
        , PC.string "oth" $> Other
        ]
    P.eof
    pure col

parsePassIDStrict :: Parser Int
parsePassIDStrict = do
    ds <- replicateM 9 PC.digitChar
    P.eof
    pure $ read ds

parseInput :: T.Text -> [M.Map T.Text T.Text]
parseInput txt =
  let
    mkKeyVal = T.breakOn ":" .> second (T.drop 1)
  in
    fmap (T.words .> fmap mkKeyVal .> M.fromList) (T.splitOn "\n\n" txt)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)
