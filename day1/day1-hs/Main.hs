{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (guard)
import Data.List (nub)
import System.IO (hGetContents, stdin)

type Expense = Int

part1 :: [Expense] -> [Expense]
part1 expenses = nub $ do
  x <- expenses
  guard $ elem (2020 - x) expenses
  return x

part2 :: [Expense] -> [Expense]
part2 expenses = nub $ do
  x <- expenses
  y <- expenses
  guard $ elem (2020 - x - y) expenses
  return x

main :: IO ()
main = do
  expenses <- fmap (read @Expense) . lines <$> hGetContents stdin

  print $ part1 expenses
  print $ part2 expenses
