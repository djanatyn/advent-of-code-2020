{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List
import System.IO

type Bound = (Int, Int)

data Action
  = Front
  | Back
  deriving (Show)

data Seat = Seat {seatRow :: [Action], seatCol :: [Action]} deriving (Show)

data Result = Result {resultRow :: Bound, resultCol :: Bound, seatID :: Int} deriving (Show)

narrow :: Bound -> Action -> Bound
narrow (min, max) Front = (min, min + half) where half = div (max - min) 2
narrow (min, max) Back = (min + half + 1, max) where half = div (max - min) 2

pAction :: Char -> Action
pAction 'F' = Front
pAction 'B' = Back
pAction 'L' = Front
pAction 'R' = Back
pAction _ = error "could not parse action"

pSeat :: String -> Seat
pSeat input = Seat {seatRow = pAction <$> row, seatCol = pAction <$> col}
  where
    row = intersect input "FB"
    col = intersect input "LR"

calcSeatID :: Bound -> Bound -> Int
calcSeatID (rowMin, rowHigh) (colMin, colHigh)
  | (rowMin == rowHigh) && (colMin == colHigh) = rowMin * 8 + colMin
  | otherwise = error "wrong bounds!"

solve :: Seat -> Result
solve Seat {seatRow, seatCol} =
  Result
    { resultRow,
      resultCol,
      seatID = calcSeatID resultRow resultCol
    }
  where
    resultRow = foldl narrow (0, 127) seatRow
    resultCol = foldl narrow (0, 7) seatCol

main :: IO ()
main = do
  input <- lines <$> hGetContents stdin
  print $ solve . pSeat <$> input
