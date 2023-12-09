{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day09 where

import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.List qualified as L


main :: IO ()
main = getInputAndSolve (parseInput parseValues) sumOfNextPredictions sumOfPrevPredictions


-- SOLVE

sumOfNextPredictions :: [Values] -> Int
sumOfNextPredictions = sum . map (predictValue $ \vals diff -> last vals + diff)


sumOfPrevPredictions :: [Values] -> Int
sumOfPrevPredictions = sum . map (predictValue $ \vals diff -> head vals - diff)


-- HELPERS

predictValue :: ([Int] -> Int -> Int) -> Values -> Int
predictValue applyDiff vs =
    if all (== 0) vs.vals
        then 0
        else
            let diffs =
                    catMaybes . snd $
                        L.mapAccumL
                            (\s v -> (Just v, (v -) <$> s))
                            Nothing
                            vs.vals
                nextDiff = predictValue applyDiff $ Values diffs
             in applyDiff vs.vals nextDiff


-- PARSE

newtype Values = Values
    { vals :: [Int]
    }
    deriving (Show)


parseValues :: ReadP Values
parseValues = Values <$> sepBy1 parseInt (char ' ')
