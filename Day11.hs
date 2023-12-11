{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.Array (Array)
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.Array qualified as A
import Data.List qualified as L


main :: IO ()
main = getInputAndSolve (parseInputRaw parseGrid) (sumPairPaths 1) (sumPairPaths 999_999)


-- SOLVE

sumPairPaths :: Int -> Grid -> Int
sumPairPaths expansionAmount g =
    let galaxyLocs = expandUniverse expansionAmount g
        pairs = concatMap (\(g1, gs) -> (g1,) <$> gs) . zip galaxyLocs $ L.tails (drop 1 galaxyLocs)
     in sum $ map calcDistance pairs
  where
    calcDistance :: ((Int, Int), (Int, Int)) -> Int
    calcDistance ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)


-- HELPERS

expandUniverse :: Int -> Grid -> [(Int, Int)]
expandUniverse expansionAmount g =
    let initialGalaxies = findGalaxies g
        (blankCols, blankRows) = bimap incrementSuccessive incrementSuccessive $ findBlanks g
     in foldl' expandRows (foldl' expandColumns initialGalaxies blankCols) blankRows
  where
    incrementSuccessive :: [Int] -> [Int]
    incrementSuccessive = zipWith (\i -> (+) (i * expansionAmount)) [0 ..]

    expandColumns :: [(Int, Int)] -> Int -> [(Int, Int)]
    expandColumns gs blankCol = map (\coords@(x, y) -> if x > blankCol then (expansionAmount + x, y) else coords) gs

    expandRows :: [(Int, Int)] -> Int -> [(Int, Int)]
    expandRows gs blankRow = map (\coords@(x, y) -> if y > blankRow then (x, expansionAmount + y) else coords) gs


findGalaxies :: Grid -> [(Int, Int)]
findGalaxies g =
    mapMaybe (fmap fst . sequence) $ A.assocs g.fromGrid


findBlanks :: Grid -> ([Int], [Int])
findBlanks Grid {fromGrid} =
    let (_, (cols, rows)) = A.bounds fromGrid
     in ( [ col
          | col <- [0 .. cols]
          , let colVals = [fromGrid A.! (col, row) | row <- [0 .. rows]]
          , null $ catMaybes colVals
          ]
        , [ row
          | row <- [0 .. rows]
          , let rowVals = [fromGrid A.! (col, row) | col <- [0 .. cols]]
          , null $ catMaybes rowVals
          ]
        )


-- PARSE

newtype Grid = Grid
    { fromGrid :: Array (Int, Int) (Maybe ())
    }
    deriving (Show)


parseGrid :: ReadP Grid
parseGrid =
    Grid
        <$> parseValueGrid (choice [Just () <$ char '#', Nothing <$ char '.'])
        <* newline
