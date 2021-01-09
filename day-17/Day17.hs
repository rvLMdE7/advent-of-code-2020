{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Day17 where

import Control.Arrow ((&&&))
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens (view)
import Control.Monad (replicateM_, when, unless, guard)
import Data.ByteString qualified as Bytes
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Traversable (for)
import Data.Void (Void)
import Flow ((.>))
import Linear (V2(V2), V3(V3), _x, _y, _z)
import ListT qualified
import StmContainers.Map qualified as STM (Map)
import StmContainers.Map qualified as STM.Map
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char


type Parser a = Parse.Parsec Void Text a

data Status = Active | Inactive
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main :: IO ()
main = do
    file <- readFileUtf8 "day-17/input.txt"
    case Parse.runParser parseGrid "day 17 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right grid -> do
            let lattice = injectAtZ 0 grid
            print =<< STM.atomically (part1 6 lattice)

part1 :: Int -> Map (V3 Int) Status -> STM Int
part1 n begin = do
    end <- part1' n begin
    pure $ length $ Map.filter (== Active) end

part1' :: Int -> Map (V3 Int) Status -> STM (Map (V3 Int) Status)
part1' n static = do
    mut <- STM.Map.new
    for_ (Map.toList static) $ \(pt, status) ->
        STM.Map.insert status pt mut
    steps n mut
    viewMap mut

steps :: Int -> STM.Map (V3 Int) Status -> STM ()
steps n lattice = replicateM_ n $ step lattice

step :: STM.Map (V3 Int) Status -> STM ()
step lattice = do
    assocs <- ListT.toList $ STM.Map.listT lattice
    actions <- for (growByOne assocs) $ \(pt, status) -> do
        let countActive = filter (== Active) .> length
        numActive <- countActive <$> neighbours pt lattice
        pure (pt, status, numActive)
    for_ actions $ \(pt, status, numActive) -> case status of
        Active -> unless (numActive `elem` [2, 3]) $ STM.Map.delete pt lattice
        Inactive -> when (numActive == 3) $ STM.Map.insert Active pt lattice
    delInactive lattice

growByOne :: [(V3 Int, Status)] -> [(V3 Int, Status)]
growByOne assocs = do
    pt <- inflateByOne $ Map.keys asMap
    pure (pt, Map.findWithDefault Inactive pt asMap)
  where
    asMap = Map.fromList assocs

inflateByOne :: [V3 Int] -> [V3 Int]
inflateByOne = concatMap withinOne .> nubOrd

numSatisfy :: (v -> Bool) -> STM.Map k v -> STM Int
numSatisfy predic dict = length <$> valsSatisfy predic dict

valsSatisfy :: (v -> Bool) -> STM.Map k v -> STM [v]
valsSatisfy predic dict = do
    assocs <- ListT.toList $ STM.Map.listT dict
    let (_keys, vals) = unzip assocs
    pure $ filter predic vals

viewMap :: Ord k => STM.Map k v -> STM (Map k v)
viewMap dict = do
    assocs <- ListT.toList $ STM.Map.listT dict
    pure $ Map.fromList assocs

neighbours :: V3 Int -> STM.Map (V3 Int) Status -> STM [Status]
neighbours orig lattice = for (neighbourPts orig) $ \pt ->
    fromMaybe Inactive <$> STM.Map.lookup pt lattice

neighbourPts :: V3 Int -> [V3 Int]
neighbourPts pt = do
    nbr <- withinOne pt
    guard $ nbr /= pt
    pure nbr

withinOne :: V3 Int -> [V3 Int]
withinOne pt = do
    delta <- V3 <$> range <*> range <*> range
    pure $ pt + delta
  where
    range = [-1, 0, 1]

injectAtZ :: Int -> Map (V2 Int) a -> Map (V3 Int) a
injectAtZ z grid = Map.fromList $ do
    (V2 x y, val) <- Map.toList grid
    pure (V3 x y z, val)

delInactive :: (Eq k, Hashable k) => STM.Map k Status -> STM ()
delInactive dict = do
    assocs <- ListT.toList $ STM.Map.listT dict
    for_ assocs $ \(key, status) ->
        when (status == Inactive) $ STM.Map.delete key dict

mapKeysMaybe :: Ord k2 => (k1 -> Maybe k2) -> Map k1 v -> Map k2 v
mapKeysMaybe maybeKey = Map.toList .> mapMaybe key .> Map.fromList
  where
    key (k, v) = (, v) <$> maybeKey k

parseTile :: Parser Status
parseTile = Parse.choice
    [ Parse.Char.char '.' $> Inactive
    , Parse.Char.char '#' $> Active
    ]

parseRow :: Parser (Map Int Status)
parseRow = do
    status <- zip [0..] <$> Parse.many parseTile
    pure $ Map.fromList status

parseGrid :: Parser (Map (V2 Int) Status)
parseGrid = do
    rows <- zip [0..] <$> Parse.sepEndBy parseRow Parse.Char.newline
    pure $ Map.fromList $ do
        (y, row) <- rows
        (x, status) <- Map.toList row
        pure (V2 x y, status)

prettyStatus :: Status -> Char
prettyStatus status = case status of
    Active -> '#'
    Inactive -> '.'

prettyGrid :: (V2 Int, V2 Int) -> Map (V2 Int) Status -> Text
prettyGrid (V2 minX minY, V2 maxX maxY) grid = Text.intercalate "\n" $ do
    y <- [minY .. maxY]
    pure $ Text.pack $ do
        x <- [minX .. maxX]
        let status = Map.findWithDefault Inactive (V2 x y) grid
        pure $ prettyStatus status

prettyLattice :: Map (V3 Int) Status -> Text
prettyLattice lattice
    | Map.null lattice = ""
    | otherwise = Text.intercalate "\n" $ do
        z <- [minZ .. maxZ]
        let mkGrid (V3 x y z') = if z' == z then Just (V2 x y) else Nothing
            grid = mapKeysMaybe mkGrid lattice
        pure $ Text.unlines
            [ Text.pack $ "z = " <> show z
            , prettyGrid (V2 minX minY, V2 maxX maxY) grid
            ]
  where
    pts = Map.keysSet lattice
    minMax = minimum &&& maximum
    (minX, maxX) = minMax $ Set.map (view _x) pts
    (minY, maxY) = minMax $ Set.map (view _y) pts
    (minZ, maxZ) = minMax $ Set.map (view _z) pts

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
