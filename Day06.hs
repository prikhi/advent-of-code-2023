{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day06 where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


main :: IO ()
main = getInputAndSolve (parseInputRaw parseRaces) productOfWaysToWin withCorrectPaper


-- SOLVE

productOfWaysToWin :: Races -> Int
productOfWaysToWin =
    product . map (length . findWins) . races


-- | This takes like 15seconds to run but I got a headache to good enough
withCorrectPaper :: Races -> Int
withCorrectPaper =
    productOfWaysToWin . Races . fixRaces . (.races)
  where
    fixRaces :: [Race] -> [Race]
    fixRaces races =
        let (t, d) = unzip $ map ((.time) &&& (.distance)) races
         in [Race (joinInts t) (joinInts d)]
    joinInts :: [Int] -> Int
    joinInts = read . concatMap show


-- HELPERS

findWins :: Race -> [Int]
findWins race =
    mapMaybe isWin [0 .. race.time]
  where
    isWin :: Int -> Maybe Int
    isWin holdTime =
        let travelLength = holdTime * (race.time - holdTime)
         in if travelLength > race.distance
                then Just travelLength
                else Nothing


-- PARSE

newtype Races = Races
    { races :: [Race]
    }
    deriving (Show)


data Race = Race
    { time :: Int
    , distance :: Int
    }
    deriving (Show)


parseRaces :: ReadP Races
parseRaces = do
    void $ string "Time:" <* many1 (char ' ')
    times <- sepBy1 parseInt (many1 $ char ' ')
    void newline
    void $ string "Distance:" <* many1 (char ' ')
    distances <- sepBy1 parseInt (many1 $ char ' ')
    void newline
    return . Races $ uncurry Race <$> zip times distances
