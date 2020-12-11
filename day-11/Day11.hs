module Day11 where

import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Flow ((.>))
import Linear (V2(V2), V3(V3))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as PC


type Parser = P.Parsec Void T.Text

data Tile = Floor | Empty | Occupied
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main :: IO ()
main = do
    file <- readFileUtf8 "day-11/input.txt"
    case P.parse (parseTiles <* P.eof) "day 11 input" file of
        Left err -> putStrLn $ P.errorBundlePretty err
        Right seats -> do
            print $ part1 seats

part1 :: M.Map (V2 Int) Tile -> Int
part1 = fixSeats .> count Occupied

count :: Eq a => a -> M.Map k a -> Int
count x = M.filter (== x) .> M.size

fixSeats :: M.Map (V2 Int) Tile -> M.Map (V2 Int) Tile
fixSeats dict = if next == dict then dict else fixSeats next
  where
    next = nextRound dict

nextRound :: M.Map (V2 Int) Tile -> M.Map (V2 Int) Tile
nextRound dict = M.mapWithKey (\pt _tile -> nextSeatAt dict pt) dict

nextSeatAt :: M.Map (V2 Int) Tile -> V2 Int -> Tile
nextSeatAt dict pt
    | seat == Empty && notElem Occupied adj = Occupied
    | seat == Occupied && length (filter (== Occupied) adj) >= 4 = Empty
    | otherwise = seat
  where
    V3 (V3 a b c) (V3 d seat e) (V3 f g h) = neighbours dict pt
    adj = [a, b, c, d, e, f, g, h]

neighbours :: M.Map (V2 Int) Tile -> V2 Int -> V3 (V3 Tile)
neighbours dict pt = V3 (row (-1)) (row 0) (row 1)
  where
    get x y = M.findWithDefault Floor (pt + V2 x y) dict
    row y = V3 (get (-1) y) (get 0 y) (get 1 y)

parseTiles :: Parser (M.Map (V2 Int) Tile)
parseTiles = do
    rows <- ix $ P.sepEndBy (ix parseTileRow) PC.newline
    pure $ M.fromList $ do
        (y, row) <- rows
        (x, tile) <- row
        pure (V2 x y, tile)
  where
    ix = fmap $ zip [0..]

parseTileRow :: Parser [Tile]
parseTileRow = P.some parseTile

parseTile :: Parser Tile
parseTile = P.choice
    [ PC.char '.' $> Floor
    , PC.char '#' $> Occupied
    , PC.char 'L' $> Empty
    ]

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path