{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Day02 where

import Control.Monad
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


main :: IO ()
main = getInputAndSolve (parseInput parseGame) possibleWithBag minimalPowerSum


-- SOLVE

possibleWithBag :: [Game] -> Int
possibleWithBag = sum . map (.gameId) . filter (all isPossible . (.sets))
  where
    isPossible :: CubeSet -> Bool
    isPossible cubeSet =
        cubeSet.red <= 12 && cubeSet.green <= 13 && cubeSet.blue <= 14


minimalPowerSum :: [Game] -> Int
minimalPowerSum = sum . map (toPower . findMinimalBag . (.sets))
  where
    findMinimalBag :: [CubeSet] -> CubeSet
    findMinimalBag sets =
        CubeSet
            { red = maximum $ map (.red) sets
            , green = maximum $ map (.green) sets
            , blue = maximum $ map (.blue) sets
            }
    toPower :: CubeSet -> Int
    toPower cubeSet =
        cubeSet.red * cubeSet.blue * cubeSet.green


-- PARSE

data Game = Game
    { gameId :: Int
    , sets :: [CubeSet]
    }
    deriving (Show)


data CubeSet = CubeSet
    { red :: Int
    , blue :: Int
    , green :: Int
    }
    deriving (Show)


combineSet :: CubeSet -> CubeSet -> CubeSet
combineSet s1 s2 =
    CubeSet
        { red = s1.red + s2.red
        , blue = s1.blue + s2.blue
        , green = s1.green + s2.green
        }


parseGame :: ReadP Game
parseGame = do
    void $ string "Game "
    gameId <- parseInt <* string ": "
    sets <- sepBy1 parseSet (string "; ")
    pure Game {..}


parseSet :: ReadP CubeSet
parseSet =
    foldl1 combineSet <$> sepBy1 parseCube (string ", ")


parseCube :: ReadP CubeSet
parseCube = do
    cubeCount <- parseInt
    skipSpaces
    choice
        [ CubeSet cubeCount 0 0 <$ string "red"
        , CubeSet 0 cubeCount 0 <$ string "blue"
        , CubeSet 0 0 cubeCount <$ string "green"
        ]
