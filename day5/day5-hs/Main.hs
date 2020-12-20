{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List

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
    row = input `intersect` "FB"
    col = input `intersect` "LR"

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
  input <- lines <$> getContents
  let process = solve . pSeat
      results = process <$> input
      seatIDs = seatID . process <$> input
      seatRange = [(minimum seatIDs) .. (maximum seatIDs)]
   in do
        print results
        putStrLn $ "problem 1: " ++ show (maximum seatIDs)
        putStrLn $ "problem 2: " ++ show (seatRange \\ seatIDs)
