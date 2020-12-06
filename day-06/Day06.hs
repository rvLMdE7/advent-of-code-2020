module Day06 where

import Data.ByteString qualified as B
import Data.List.NonEmpty qualified as N
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC


type Parser = P.Parsec Void T.Text

main :: IO ()
main = do
    file <- readFileUtf8 "day-06/input.txt"
    case P.parse (parseGroups <* P.eof) "day 6 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right input -> do
            print $ part1 input
            print $ part2 input

part1 :: [N.NonEmpty (S.Set Char)] -> Int
part1 = fmap (S.unions .> S.size) .> sum

part2 :: [N.NonEmpty (S.Set Char)] -> Int
part2 = fmap (intersections .> S.size) .> sum

intersections :: Ord a => N.NonEmpty (S.Set a) -> S.Set a
intersections (set N.:| sets) = foldr S.intersection set sets

parseGroups :: Parser [N.NonEmpty (S.Set Char)]
parseGroups = parseGroup `P.sepEndBy` PC.newline

parseGroup :: Parser (N.NonEmpty (S.Set Char))
parseGroup = N.fromList <$> P.sepEndBy1 parseAnswers PC.newline

parseAnswers :: Parser (S.Set Char)
parseAnswers = S.fromList <$> P.some (P.oneOf ['a' .. 'z'])

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path
