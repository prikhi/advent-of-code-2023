{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Day05 where

import Control.Monad
import Data.Either
import Data.Foldable
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.List qualified as L


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main = getInputAndSolve (parseInputRaw parseAlmanac) lowestLocation lowestLocationWithSeedRange


-- SOLVE

lowestLocation :: Almanac -> Int
lowestLocation Almanac {..} =
    minimum $
        map
            ( \start ->
                foldl'
                    applyMapping
                    start
                    [ seedToSoil
                    , soilToFert
                    , fertToWater
                    , waterToLight
                    , lightToTemp
                    , tempToHumid
                    , humidToLoc
                    ]
            )
            seeds


lowestLocationWithSeedRange :: Almanac -> Int
lowestLocationWithSeedRange Almanac {..} =
    let fixedSeeds =
            map (toSeedRange . map snd) $
                L.groupBy (\(i1, _) (i2, _) -> i1 `div` (2 :: Int) == i2 `div` 2) $
                    zip [0 ..] seeds
     in minimum . map (.start) $
            foldl'
                ( \acc mapping ->
                    concatMap (applyMappingToRange mapping) acc
                )
                fixedSeeds
                [ seedToSoil
                , soilToFert
                , fertToWater
                , waterToLight
                , lightToTemp
                , tempToHumid
                , humidToLoc
                ]
  where
    toSeedRange :: [Int] -> SeedRange
    toSeedRange = \case
        [start, len] -> SeedRange {..}
        _ -> error "unexpected"


-- HELPERS

applyMapping :: Int -> [Mapping] -> Int
applyMapping start maps =
    fromLeft start $
        foldM
            ( \() mapping ->
                if start >= mapping.sourceStart && start <= mapping.sourceStart + mapping.mappingLength - 1
                    then
                        let depthIntoMap = start - mapping.sourceStart
                         in Left $ mapping.destinationStart + depthIntoMap
                    else Right ()
            )
            ()
            maps


applyMappingToRange :: [Mapping] -> SeedRange -> [SeedRange]
applyMappingToRange mappings initialRange =
    uncurry (<>) $ foldl' goMaps ([], [initialRange]) mappings
  where
    -- apply mapping to untransformed ranges
    goMaps :: ([SeedRange], [SeedRange]) -> Mapping -> ([SeedRange], [SeedRange])
    goMaps (doneRanges, untouchedRanges) m =
        let (newDone, newUntouched) = foldl' (goMap m) ([], []) untouchedRanges
         in (newDone <> doneRanges, newUntouched)
    -- find & shift overlaps between mapping & range
    goMap :: Mapping -> ([SeedRange], [SeedRange]) -> SeedRange -> ([SeedRange], [SeedRange])
    goMap m (done, untouched) s =
        let (overlapping, noOverlap) =
                findOverlaps
                    (m.sourceStart, m.sourceStart + m.mappingLength - 1)
                    (s.start, s.start + s.len - 1)
         in (map (shift m) overlapping <> done, map retain noOverlap <> untouched)
    -- split into overlap ranges & no overlap ranges
    findOverlaps :: (Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
    findOverlaps (mStart, mEnd) s@(sStart, sEnd) =
        if
            | (sStart >= mStart && sEnd <= mEnd) ->
                -- whole seed range in map range
                ([s], [])
            | sEnd >= mStart && sEnd <= mEnd ->
                -- end of seed range in map range
                ([(mStart, sEnd)], [(sStart, mStart - 1)])
            | sStart >= mStart && sStart <= mEnd ->
                -- start of seed range in map range
                ([(sStart, mEnd)], [(mEnd + 1, sEnd)])
            | sStart <= mStart && sEnd >= mEnd ->
                -- middle of seed range in map range
                ([(mStart, mEnd)], [(sStart, mStart - 1), (mEnd + 1, sEnd)])
            | otherwise ->
                -- no overlap
                ([], [s])
    -- shift to dest range & transform back into SeedRange
    shift :: Mapping -> (Int, Int) -> SeedRange
    shift Mapping {..} (sStart, sEnd) =
        let len = sEnd - sStart + 1
            depthIntoMap = sStart - sourceStart
         in SeedRange {start = destinationStart + depthIntoMap, len}
    -- just transform back into seed range
    retain :: (Int, Int) -> SeedRange
    retain (sStart, sEnd) =
        SeedRange
            { start = sStart
            , len = sEnd - sStart + 1
            }


data SeedRange = SeedRange
    { start :: Int
    , len :: Int
    }
    deriving (Show)


-- PARSE

data Almanac = Almanac
    { seeds :: [Int]
    , seedToSoil :: [Mapping]
    , soilToFert :: [Mapping]
    , fertToWater :: [Mapping]
    , waterToLight :: [Mapping]
    , lightToTemp :: [Mapping]
    , tempToHumid :: [Mapping]
    , humidToLoc :: [Mapping]
    }
    deriving (Show)


parseAlmanac :: ReadP Almanac
parseAlmanac = do
    let parseSpacer = void $ newline *> newline
    void $ string "seeds: "
    seeds <- sepBy1 parseInt (satisfy (== ' '))
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    seedToSoil <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    soilToFert <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    fertToWater <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    waterToLight <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    lightToTemp <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    tempToHumid <- sepBy1 parseMapping newline
    parseSpacer
    void $ many1 $ satisfy (/= '\n')
    void newline
    humidToLoc <- sepBy1 parseMapping newline
    void newline
    return Almanac {..}


data Mapping = Mapping
    { destinationStart :: Int
    , sourceStart :: Int
    , mappingLength :: Int
    }
    deriving (Show)


parseMapping :: ReadP Mapping
parseMapping = do
    destinationStart <- parseInt
    void $ char ' '
    sourceStart <- parseInt
    void $ char ' '
    mappingLength <- parseInt
    return Mapping {..}
