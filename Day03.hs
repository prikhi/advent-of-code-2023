{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Day03 where

import Control.Arrow ((&&&))
import Data.Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

import Harness
import ParseHelper

import Data.Array qualified as A
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL


main :: IO ()
main = getInputAndSolve (parseInputRaw parsePartNumbers) calcPartNumberSum calcGearRatioSum


-- SOLVE

calcPartNumberSum :: [PartNumber] -> Int
calcPartNumberSum =
    sum . map (.fullNumber) . filter (.symbolAdjacent)


calcGearRatioSum :: [PartNumber] -> Int
calcGearRatioSum =
    sum
        . map calcRatio
        . filter isPair
        . L.groupBy ((==) `on` fst)
        . L.sortOn fst
        . concatMap (\pn -> (,pn) <$> pn.adjacentStars)
  where
    isPair :: [a] -> Bool
    isPair = \case
        [_, _] -> True
        _ -> False
    calcRatio :: [(a, PartNumber)] -> Int
    calcRatio = product . map ((.fullNumber) . snd)


-- HELPERS

extractPartNumbers :: PointGrid -> [PartNumber]
extractPartNumbers pg@(PointGrid grid) =
    let ((xMin, yMin), (xMax, yMax)) = A.bounds grid
     in concat
            [ findPartNumbersInRow pg row $
                catMaybes
                    [ case grid A.! (col, row) of
                        Digit i -> Just (i, col)
                        _ -> Nothing
                    | col <- [xMin .. xMax]
                    ]
            | row <- [yMin .. yMax]
            ]


findPartNumbersInRow :: PointGrid -> Int -> [(Int, Int)] -> [PartNumber]
findPartNumbersInRow (PointGrid grid) row = buildLastNumber . foldl' go ([], Nothing)
  where
    go :: ([PartNumber], Maybe (NonEmpty (Int, Int))) -> (Int, Int) -> ([PartNumber], Maybe (NonEmpty (Int, Int)))
    go (builtNumbers, inProgress) this@(_, col) = case inProgress of
        Nothing ->
            (builtNumbers, Just $ return this)
        Just nel ->
            let prevCol = snd $ NEL.last nel
             in if col /= succ prevCol
                    then (buildNumber nel : builtNumbers, Just $ return this)
                    else (builtNumbers, Just $ nel <> return this)
    buildLastNumber :: ([PartNumber], Maybe (NonEmpty (Int, Int))) -> [PartNumber]
    buildLastNumber (built, mbToBuild) = case mbToBuild of
        Nothing -> built
        Just toBuild -> buildNumber toBuild : built
    buildNumber :: NonEmpty (Int, Int) -> PartNumber
    buildNumber digits =
        let cols = snd <$> digits
            neighbors =
                map (id &&& (grid A.!))
                    . L.nub
                    $ concatMap
                        (\col -> A.getGridNeighborsDiagonal grid (col, row))
                        cols
         in PartNumber
                { fullNumber = digitListToNumber . NEL.toList $ fmap fst digits
                , symbolAdjacent = any (hasSymbol . snd) neighbors
                , adjacentStars = map fst $ filter ((== Symbol '*') . snd) neighbors
                }


digitListToNumber :: [Int] -> Int
digitListToNumber = foldl' (\acc i -> acc * 10 + i) 0


-- PARSE

data PartNumber = PartNumber
    { fullNumber :: Int
    , symbolAdjacent :: Bool
    , adjacentStars :: [(Int, Int)]
    }
    deriving (Eq, Show)


parsePartNumbers :: ReadP [PartNumber]
parsePartNumbers = do
    !pg <- parsePointGrid
    return $ extractPartNumbers pg


data Point
    = Digit Int
    | Symbol Char
    | Blank
    deriving (Eq, Show)


hasSymbol :: Point -> Bool
hasSymbol = \case
    Symbol _ -> True
    _ -> False


newtype PointGrid
    = PointGrid (A.Array (Int, Int) Point)


instance Show PointGrid where
    show (PointGrid g) = showPointGrid g


showPointGrid :: A.Array (Int, Int) Point -> String
showPointGrid = A.showGridWith $ \case
    Blank -> "."
    Symbol c -> [c]
    Digit i -> show i


parsePointGrid :: ReadP PointGrid
parsePointGrid = do
    PointGrid . fmap parseChar <$> parseCharGrid validChar <* newline
  where
    validChar :: Char -> Bool
    validChar = (/= '\n')
    parseChar :: Char -> Point
    parseChar c = case readMaybe [c] of
        Just i -> Digit i
        Nothing -> case c of
            '.' -> Blank
            _ -> Symbol c
