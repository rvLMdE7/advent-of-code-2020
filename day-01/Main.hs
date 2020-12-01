import Control.Monad (guard)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "input.txt"
    print $ part1 input

part1 :: [Int] -> Int
part1 = findPairSumsTo 2020 .> uncurry (*)

findPairSumsTo :: (Eq a, Num a) => a -> [a] -> (a, a)
findPairSumsTo target list = head $ do
    x <- list
    y <- list
    guard $ x + y == target
    pure (x, y)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> [Int]
parseInput = T.lines .> fmap readInt
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
