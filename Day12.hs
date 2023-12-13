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

module Day12 where

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
main = getInputAndSolve (parseInput parseRecord) sumPossiblities sumUnfoldedPossibilities


-- SOLVE

sumPossiblities :: [Record] -> Int
sumPossiblities = sum . map (length . possibilities)


-- | WIP: naive version takes too long
sumUnfoldedPossibilities :: [Record] -> Int
sumUnfoldedPossibilities = sumPossiblities . map unfoldRecord
  where
    unfoldRecord :: Record -> Record
    unfoldRecord r =
        Record
            { springs = A.fromList . L.intercalate [Unknown] . replicate 5 $ toList r.springs
            , damagedGroups = concat $ replicate 5 r.damagedGroups
            }


-- HELPERS

possibilities :: Record -> [Array Int Spring]
possibilities r = do
    let vals = A.assocs r.springs
        knownDamaged = map fst $ filter ((== Damaged) . snd) vals
        totalDamaged = sum r.damagedGroups
        missingDamaged = totalDamaged - length knownDamaged
        unknowns = map fst $ filter ((== Unknown) . snd) vals
    slots <- [s | s <- L.subsequences unknowns, length s == missingDamaged]
    let newOperational = unknowns L.\\ slots
        newSpring = A.set (((,Operational) <$> newOperational) <> ((,Damaged) <$> slots)) r.springs
    guard $ validate newSpring
    return $ newSpring
  where
    validate :: Array Int Spring -> Bool
    validate =
        (== r.damagedGroups) . map length . filter (any (== Damaged)) . L.group . A.elems


-- PARSE

data Record = Record
    { springs :: Array Int Spring
    , damagedGroups :: [Int]
    }
    deriving (Show, Eq)


parseRecord :: ReadP Record
parseRecord = do
    springs <- A.fromList <$> many1 parseSpring
    void $ char ' '
    damagedGroups <- sepBy1 parseInt (char ',')
    return Record {..}


data Spring
    = Operational
    | Damaged
    | Unknown
    deriving (Show, Eq)


parseSpring :: ReadP Spring
parseSpring =
    choice
        [ Operational <$ char '.'
        , Damaged <$ char '#'
        , Unknown <$ char '?'
        ]
