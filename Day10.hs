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

module Day10 where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Array (Array)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.Array qualified as A
import Data.List qualified as L
import Data.Map qualified as M
import Data.MultiSet qualified as MS
import Data.Set qualified as S

import Debug.Trace


main :: IO ()
main = getInputAndSolve (parseInputRaw parseGrid) findMidPoint (const "Implement Part 2")


-- SOLVE

findMidPoint :: Grid -> Int
findMidPoint g =
    let start = findStart g
     in length (followPipe g [] start) `div` 2


enclosedTiles :: Grid -> Int
enclosedTiles g =
    let start@(startIx, _) = findStart g
        pipeIxs = startIx : followPipe g [] start
        groundIxs = map fst . filter ((== Ground) . snd) $ A.assocs g.fromGrid
     in 0


-- HELPERS

findStart :: Grid -> ((Int, Int), Direction)
findStart g =
    let startIx = fst . fromJust . L.find ((== Start) . snd) $ A.assocs g.fromGrid
     in {- TODO: calculate first direction to move from start
        validDirs =
            [ ((East, (+ 1), (+ 0)), [NorthWest, SouthWest, EastWest])
            , ((West, \x -> x - 1, (+ 0)), [NorthEast, SouthEast, EastWest])
            , ((North, (+ 0), (+ 1)), [NorthSouth, SouthWest, SouthEast])
            , ((South, (+ 0), \y -> y - 1), [NorthSouth, NorthEast, NorthWest])
            ]
        neighbors = A.getGridNeighborsCardinal g.fromGrid startIx
        nextDir = L.find ...
        -}
        (startIx, South)


followPipe :: Grid -> [(Int, Int)] -> ((Int, Int), Direction) -> [(Int, Int)]
followPipe g seen (nextIx, fromDir) =
    let moveNorth = (second (\y -> y - 1) nextIx, South)
        moveSouth = (second (+ 1) nextIx, North)
        moveEast = (first (+ 1) nextIx, West)
        moveWest = (first (\x -> x - 1) nextIx, East)
        keepMoving = followPipe g (nextIx : seen)
     in case (g.fromGrid A.! nextIx, fromDir) of
            (Start, _) ->
                if
                    | not (null seen) -> seen
                    | fromDir == South -> keepMoving moveSouth
                    | fromDir == North -> keepMoving moveNorth
                    | fromDir == East -> keepMoving moveEast
                    | fromDir == West -> keepMoving moveWest
                    | otherwise -> error "unexpected"
            (Ground, _) -> error "got to ground?"
            (NorthSouth, South) -> keepMoving moveNorth
            (NorthSouth, North) -> keepMoving moveSouth
            (EastWest, East) -> keepMoving moveWest
            (EastWest, West) -> keepMoving moveEast
            (NorthEast, North) -> keepMoving moveEast
            (NorthEast, East) -> keepMoving moveNorth
            (NorthWest, North) -> keepMoving moveWest
            (NorthWest, West) -> keepMoving moveNorth
            (SouthEast, South) -> keepMoving moveEast
            (SouthEast, East) -> keepMoving moveSouth
            (SouthWest, South) -> keepMoving moveWest
            (SouthWest, West) -> keepMoving moveSouth
            other -> error $ "Unexpected: " <> show other <> " via " <> show seen


data Direction
    = North
    | South
    | East
    | West
    deriving (Show, Eq)


-- PARSE

newtype Grid = Grid
    { fromGrid :: Array (Int, Int) Tile
    }
    deriving (Show)


parseGrid :: ReadP Grid
parseGrid =
    Grid <$> parseValueGrid parseTile <* newline


data Tile
    = Ground
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Start
    deriving (Show, Eq)


parseTile :: ReadP Tile
parseTile =
    choice $
        map
            (\(t, c) -> t <$ char c)
            [ (NorthSouth, '|')
            , (EastWest, '-')
            , (NorthEast, 'L')
            , (NorthWest, 'J')
            , (SouthWest, '7')
            , (SouthEast, 'F')
            , (Ground, '.')
            , (Start, 'S')
            ]
