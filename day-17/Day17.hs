{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Day17 where

import Control.Arrow ((&&&))
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
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
import Data.Vector qualified as Vec
import Data.Void (Void)
import Flow ((.>))
import GHC.TypeLits (type (+))
import Linear.V (Dim, dim, V(toVector, V))
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
            let lattice = injectInit 0 grid
                hyper = injectInit 0 lattice
            print =<< STM.atomically (part 6 lattice)
            print =<< STM.atomically (part 6 hyper)

part :: Dim n => Int -> Map (V n Int) Status -> STM Int
part i begin = do
    end <- part' i begin
    pure $ length $ Map.filter (== Active) end

part' :: Dim n => Int -> Map (V n Int) Status -> STM (Map (V n Int) Status)
part' i static = do
    mut <- STM.Map.new
    for_ (Map.toList static) $ \(pt, status) ->
        STM.Map.insert status pt mut
    steps i mut
    viewMap mut

steps :: Dim n => Int -> STM.Map (V n Int) Status -> STM ()
steps i lattice = replicateM_ i $ step lattice

step :: Dim n => STM.Map (V n Int) Status -> STM ()
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

growByOne :: Dim n => [(V n Int, Status)] -> [(V n Int, Status)]
growByOne assocs = do
    pt <- inflateByOne $ Map.keys asMap
    pure (pt, Map.findWithDefault Inactive pt asMap)
  where
    asMap = Map.fromList assocs

inflateByOne :: Dim n => [V n Int] -> [V n Int]
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

neighbours :: Dim n => V n Int -> STM.Map (V n Int) Status -> STM [Status]
neighbours orig lattice = for (neighbourPts orig) $ \pt ->
    fromMaybe Inactive <$> STM.Map.lookup pt lattice

neighbourPts :: Dim n => V n Int -> [V n Int]
neighbourPts pt = do
    nbr <- withinOne pt
    guard $ nbr /= pt
    pure nbr

withinOne :: Dim n => V n Int -> [V n Int]
withinOne pt = do
    delta <- V <$> Vec.replicateM dimen range
    pure $ pt + delta
  where
    dimen = dim pt
    range = [-1, 0, 1]

injectInit :: Dim n => Int -> Map (V n Int) a -> Map (V (n + 1) Int) a
injectInit z grid = Map.fromList $ do
    (V vec, val) <- Map.toList grid
    pure (V $ Vec.snoc vec z, val)

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

parseGrid :: Parser (Map (V 2 Int) Status)
parseGrid = do
    rows <- zip [0..] <$> Parse.sepEndBy parseRow Parse.Char.newline
    pure $ Map.fromList $ do
        (y, row) <- rows
        (x, status) <- Map.toList row
        pure (V $ Vec.fromList [x, y], status)

prettyStatus :: Status -> Char
prettyStatus status = case status of
    Active -> '#'
    Inactive -> '.'

prettyGrid :: (Int, Int) -> (Int, Int) -> Map (V 2 Int) Status -> Text
prettyGrid (minX, minY) (maxX, maxY) grid = Text.intercalate "\n" $ do
    y <- [minY .. maxY]
    pure $ Text.pack $ do
        x <- [minX .. maxX]
        let pt = V $ Vec.fromList [x, y]
            status = Map.findWithDefault Inactive pt grid
        pure $ prettyStatus status

prettyLattice :: Map (V 3 Int) Status -> Text
prettyLattice lattice
    | Map.null lattice = ""
    | otherwise = Text.intercalate "\n" $ do
        z <- [minZ .. maxZ]
        let mkGrid (V vec) = if Vec.last vec == z
                then Just (V $ Vec.init vec)
                else Nothing
            grid = mapKeysMaybe mkGrid lattice
        pure $ Text.unlines
            [ Text.pack $ "z = " <> show z
            , prettyGrid (minX, minY) (maxX, maxY) grid
            ]
  where
    pts = Set.map toVector $ Map.keysSet lattice
    minMax = minimum &&& maximum
    (minX, maxX) = minMax $ Set.map (Vec.! 0) pts
    (minY, maxY) = minMax $ Set.map (Vec.! 1) pts
    (minZ, maxZ) = minMax $ Set.map (Vec.! 2) pts

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Bytes.readFile path
