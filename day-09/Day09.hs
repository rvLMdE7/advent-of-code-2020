module Day09 where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))
import Data.Maybe (isJust, listToMaybe)
import Control.Monad (guard)


main :: IO ()
main = do
    xmas <- parseXmas <$> readFileUtf8 "day-09/input.txt"
    print $ part1 xmas

part1 :: VU.Vector Int -> Maybe Int
part1 = firstValNoDistinctPairSumsToInPrevN 25

firstValNoDistinctPairSumsToInPrevN
    :: (Eq a, Num a, VU.Unbox a) => Int -> VU.Vector a -> Maybe a
firstValNoDistinctPairSumsToInPrevN n vec = do
    i <- firstIndexNoDistinctPairSumsToInPrevN n vec
    pure (vec VU.! i)

firstIndexNoDistinctPairSumsToInPrevN
    :: (Eq a, Num a, VU.Unbox a) => Int -> VU.Vector a -> Maybe Int
firstIndexNoDistinctPairSumsToInPrevN n vec = listToMaybe $ do
    i <- [n .. len - 1]
    let slice = VU.slice (i - n) n vec
    guard $ not $ existsDistinctPairSumsTo (vec VU.! i) slice
    pure i
  where
    len = VU.length vec

existsDistinctPairSumsTo
    :: (Eq a, Num a, VU.Unbox a) => a -> VU.Vector a -> Bool
existsDistinctPairSumsTo target = findDistinctPairSumsTo target .> isJust

findDistinctPairSumsTo
    :: (Eq a, Num a, VU.Unbox a) => a -> VU.Vector a -> Maybe (a, a)
findDistinctPairSumsTo target =
    distinctPairs .> filter (sumsTo target) .> listToMaybe
  where
    sumsTo x (a, b) = x == a + b

distinctPairs :: (Eq a, VU.Unbox a) => VU.Vector a -> [(a, a)]
distinctPairs vec = filter distinct allPairs
  where
    allPairs = let v = VU.toList vec in (,) <$> v <*> v
    distinct = uncurry (/=)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseXmas :: T.Text -> VU.Vector Int
parseXmas = T.lines .> fmap (TR.decimal .> extract) .> VU.fromList
  where
    extract e = case e of
        Left err -> error err
        Right x -> fst x
