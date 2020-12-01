import Control.Monad (replicateM, guard)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "input.txt"
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Int
part1 = findNValsThatSumsTo 2 2020 .> product

part2 :: [Int] -> Int
part2 = findNValsThatSumsTo 3 2020 .> product

findNValsThatSumsTo :: (Eq a, Num a) => Int -> a -> [a] -> [a]
findNValsThatSumsTo n target list = head $ do
    subset <- replicateM n list
    guard $ sum subset == target
    pure subset

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> [Int]
parseInput = T.lines .> fmap readInt
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
