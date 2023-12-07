{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Day07 where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Char
import Data.Function (on)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.List qualified as L


main :: IO ()
main = getInputAndSolve (parseInput parseHand) calcTotalWinnings (calcTotalWinnings . subJokers)


-- SOLVE

calcTotalWinnings :: [Hand] -> Int
calcTotalWinnings =
    sum
        . zipWith calcWinnings [1 ..]
        . concatMap (L.sortOn (.cards))
        . sortAndGroupOn (jokerCardsToType . (.cards))
  where
    calcWinnings :: Int -> Hand -> Int
    calcWinnings rank hand = rank * hand.bet
    sortAndGroupOn :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
    sortAndGroupOn f =
        map (map snd)
            . L.groupBy ((==) `on` fst)
            . L.sortOn fst
            . map (f &&& id)


-- HELPERS

data HandType
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfKind
    | FullHouse
    | FourOfKind
    | FiveOfKind
    deriving (Show, Eq, Ord, Enum, Bounded)


cardsToType :: [Card] -> HandType
cardsToType (L.group . L.sort -> gs) =
    if
        | anyLengthIs 5 -> FiveOfKind
        | anyLengthIs 4 -> FourOfKind
        | anyLengthIs 3 && anyLengthIs 2 -> FullHouse
        | anyLengthIs 3 -> ThreeOfKind
        | length (filter ((== 2) . length) gs) == 2 -> TwoPair
        | anyLengthIs 2 -> OnePair
        | otherwise -> HighCard
  where
    anyLengthIs :: Int -> Bool
    anyLengthIs i = any ((== i) . length) gs


jokerCardsToType :: [Card] -> HandType
jokerCardsToType cards =
    let (jokers, nonJokers) = L.partition (== Joker) cards
     in foldr
            (\_ handType -> increment handType)
            (cardsToType nonJokers)
            jokers
  where
    increment :: HandType -> HandType
    increment = \case
        FiveOfKind -> FiveOfKind
        FourOfKind -> FiveOfKind
        FullHouse -> FourOfKind
        ThreeOfKind -> FourOfKind
        TwoPair -> FullHouse
        OnePair -> ThreeOfKind
        HighCard -> OnePair


subJokers :: [Hand] -> [Hand]
subJokers = map (\h -> h {cards = map substituteJoker h.cards})
  where
    substituteJoker :: Card -> Card
    substituteJoker = \case
        Jack -> Joker
        c -> c


-- PARSE

data Hand = Hand
    { cards :: [Card]
    , bet :: Int
    }
    deriving (Show)


parseHand :: ReadP Hand
parseHand = do
    cards <- many1 parseCard
    void $ char ' '
    bet <- parseInt
    return Hand {..}


data Card
    = Joker
    | Numbered Int
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Show, Ord, Eq)


parseCard :: ReadP Card
parseCard =
    choice
        [ Numbered . read . (: []) <$> satisfy isNumber
        , Ten <$ char 'T'
        , Jack <$ char 'J'
        , Queen <$ char 'Q'
        , King <$ char 'K'
        , Ace <$ char 'A'
        ]
