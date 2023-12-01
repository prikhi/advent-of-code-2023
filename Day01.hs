{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day01 where

import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


-- this one uses a different parser for pt 1 & pt 2, so need to swap these lines
main :: IO ()
main =
    -- getInputAndSolve (parseInput parseDigitsPt1) literalCalibrationValueSum (const "swap commented lines for pt2")
    getInputAndSolve (parseInput parseDigitsPt2) (const "swap commented lines for pt1") realCalibrationValueSum


-- SOLVE

literalCalibrationValueSum :: [Digits] -> Int
literalCalibrationValueSum = sum . map toCalibrationValue


realCalibrationValueSum :: [Digits] -> Int
realCalibrationValueSum = sum . map toCalibrationValue


-- HELPERS

toCalibrationValue :: Digits -> Int
toCalibrationValue digits =
    asInt (firstDigit digits) * 10 + asInt (lastDigit digits)


-- PARSE

data Digits = Digits
    { firstDigit :: Digit
    , lastDigit :: Digit
    }
    deriving (Show)


data Digit
    = Literal Int
    | Spelt Int
    deriving (Show)


onlyLiterals :: [Digit] -> [Int]
onlyLiterals = mapMaybe $ \case
    Literal i -> Just i
    Spelt _ -> Nothing


asInt :: Digit -> Int
asInt = \case
    Literal i -> i
    Spelt i -> i


parseDigitsPt1 :: ReadP Digits
parseDigitsPt1 = do
    digitList <- map Literal . onlyLiterals <$> parseDigitList
    return $ Digits (head digitList) (last digitList)


parseDigitsPt2 :: ReadP Digits
parseDigitsPt2 = do
    -- parse the whole line
    allChars <- many1 $ satisfy isAlphaNum
    -- get first digit by parsing from left to right
    let firstDigit = head . fromJust $ runParser parseDigitList allChars
    -- get last digit by parsing from right to left
    -- use longer and longer substrings until a parse succeeds
    let ltrSubstrings = drop 1 $ reverse $ tails allChars
        ltrResults = mapMaybe (runParser parseDigitList) ltrSubstrings
        lastDigit = head $ head $ filter (not . null) ltrResults
    return Digits {..}


parseDigitList :: ReadP [Digit]
parseDigitList = do
    fmap catMaybes
        . many1
        $ (Just <$> parseDigit)
            <++ (Nothing <$ satisfy isAlpha)
  where
    parseDigit :: ReadP Digit
    parseDigit =
        choice
            [ Literal . read . (: []) <$> satisfy isNumber
            , Spelt 1 <$ string "one"
            , Spelt 2 <$ string "two"
            , Spelt 3 <$ string "three"
            , Spelt 4 <$ string "four"
            , Spelt 5 <$ string "five"
            , Spelt 6 <$ string "six"
            , Spelt 7 <$ string "seven"
            , Spelt 8 <$ string "eight"
            , Spelt 9 <$ string "nine"
            ]
