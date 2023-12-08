{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Day08 where

import Control.Monad
import Data.Char
import Data.Map (Map)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.Map qualified as M


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main = getInputAndSolve (parseInputRaw parseNetwork) stepsTilZZZs spookyTime


-- SOLVE

stepsTilZZZs :: Network -> Int
stepsTilZZZs network =
    either id (error . ("Unexpected: " <>) . show) $
        foldM
            (followLinks network.links (== "ZZZ"))
            ("AAA", 0)
            (cycle network.steps)


spookyTime :: Network -> Int
spookyTime network =
    let nodesThatEndInA = filter endsInA $ M.keys network.links
        getStepToEndInZ node =
            either id (error . ("Unexpected: " <>) . show) $
                foldM
                    (followLinks network.links endsInZ)
                    (node, 0)
                    (cycle network.steps)
        stepsToZ = map getStepToEndInZ nodesThatEndInA
     in foldr lcm 1 stepsToZ
  where
    endsInA :: String -> Bool
    endsInA = \case
        [_, _, 'A'] -> True
        _ -> False
    endsInZ :: String -> Bool
    endsInZ = \case
        [_, _, 'Z'] -> True
        _ -> False


-- HELPERS

followLinks
    :: Map String (String, String)
    -> (String -> Bool)
    -> (String, Int)
    -> Direction
    -> Either Int (String, Int)
followLinks linkMap isEnd (currentLocation, stepCount) nextDirection =
    if isEnd currentLocation
        then Left stepCount
        else case M.lookup currentLocation linkMap of
            Nothing ->
                error $ "Loc not found: " <> currentLocation
            Just (l, r) ->
                if nextDirection == RightD then Right (r, succ stepCount) else Right (l, succ stepCount)


-- PARSE

data Network = Network
    { steps :: [Direction]
    , links :: M.Map String (String, String)
    }
    deriving (Show)


parseNetwork :: ReadP Network
parseNetwork = do
    !steps <- many1 parseDirection <* newline <* newline
    links <- M.fromList <$> endBy1 parseLink newline
    eof
    return Network {..}
  where
    parseLink :: ReadP (String, (String, String))
    parseLink = do
        key <- many1 (satisfy isAlphaNum)
        void $ string " = ("
        left <- many1 (satisfy isAlphaNum)
        void $ string ", "
        right <- many1 (satisfy isAlphaNum)
        void $ string ")"
        return (key, (left, right))


data Direction
    = RightD
    | LeftD
    deriving (Show, Eq)


parseDirection :: ReadP Direction
parseDirection =
    choice
        [ RightD <$ char 'R'
        , LeftD <$ char 'L'
        ]
