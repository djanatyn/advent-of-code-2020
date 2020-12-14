module Main where

type Bound = (Int, Int)

data Action
  = Front
  | Back
  deriving (Show)

narrow :: Action -> Bound -> Bound
narrow Front (min, max) = (min, min + half) where half = div (max - min) 2
narrow Back (min, max) = (min + half + 1, max) where half = div (max - min) 2

pAction :: Char -> Action
pAction 'F' = Front
pAction 'B' = Back
pAction 'L' = Front
pAction 'R' = Back
pAction _ = error "could not parse action"

main :: IO ()
main =
  print $
    scanl (flip narrow) (0, 127) $
      pAction <$> "BFFFBBF"
