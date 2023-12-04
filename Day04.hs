{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Day04 where

import Control.Monad
import Control.Monad.ST
import Data.Array (Array)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.Array qualified as A
import Data.List qualified as L


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main = getInputAndSolve (parseInput parseCard) totalPointsWon countScratchCards


-- SOLVE

totalPointsWon :: [Card] -> Int
totalPointsWon = sum . map calculatePoints


countScratchCards :: [Card] -> Int
countScratchCards = sum . A.elems . playCards


-- HELPERS

calculatePoints :: Card -> Int
calculatePoints c =
    let matches = c.winningNumbers `L.intersect` c.ourNumbers
     in if null matches
            then 0
            else 2 ^ (length matches - 1)


playCards :: [Card] -> Array Int Int
playCards cards = runST $ do
    arr <- A.newSTArray (1, maxIx) 1
    foldM_ (playCard arr) () cards
    A.freezeSTArray arr
  where
    maxIx :: Int
    maxIx =
        length cards
    playCard :: A.STArray s Int Int -> () -> Card -> ST s ()
    playCard arr _ card = do
        let matches = card.winningNumbers `L.intersect` card.ourNumbers
            nextIds =
                [card.cardId + 1 .. card.cardId + length matches]
        cardCount <- A.readSTArray arr card.cardId
        forM_ nextIds $ \nextId -> when (nextId <= maxIx) $ do
            nextCount <- A.readSTArray arr nextId
            A.writeSTArray arr nextId (nextCount + cardCount)


-- PARSE

data Card = Card
    { cardId :: Int
    , winningNumbers :: [Int]
    , ourNumbers :: [Int]
    }
    deriving (Show)


parseCard :: ReadP Card
parseCard = do
    void $ string "Card" <* many1 parseSpace
    cardId <- parseInt
    void $ string ":" <* many1 parseSpace
    winningNumbers <- sepBy1 parseInt (many1 parseSpace)
    void $ many1 parseSpace *> string "|" <* many1 parseSpace
    ourNumbers <- sepBy1 parseInt (many1 parseSpace)
    return Card {..}
  where
    parseSpace :: ReadP Char
    parseSpace = satisfy (== ' ')
