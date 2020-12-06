{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Coerce (coerce)
import Data.Maybe (catMaybes, isJust)
import System.IO (hGetContents, readFile, stdin)

data Tile
  = -- '#'
    Tree
  | -- '.'
    Square
  deriving (Show, Eq, Ord)

-- a Hill is a row
newtype Hill = Hill [Tile] deriving (Show)

-- Hills form Mountain
newtype Mountain = Mountain [Hill] deriving (Show)

-- You can look at a Mountain
newtype Coordinate = Coordinate (Int, Int) deriving (Show)

-- Anywhere you look, you can ride
newtype Toboggan = Toboggan Coordinate deriving (Show)

-- Derivative of position
newtype Slope = Slope Coordinate deriving (Show)

-- There is nothing but the Mountain
data World = World
  { navigator :: Toboggan,
    pitch :: Slope,
    plane :: Mountain
  }
  deriving (Show)

-- the Truth is the World and the Tile we stand on
type Truth = Maybe (World, Tile)

-- zero-indexed
lookupMountain :: Int -> Mountain -> Maybe Hill
lookupMountain _ (Mountain []) = Nothing
lookupMountain distance (Mountain (hill : hills))
  | distance == 0 = Just hill
  | otherwise = lookupMountain (distance - 1) (Mountain hills)

lookupHill :: Int -> Hill -> Maybe Tile
lookupHill _ (Hill []) = Nothing
lookupHill distance (Hill (tile : tiles))
  | distance == 0 = Just tile
  | otherwise = lookupHill (distance - 1) (Hill tiles)

lookupTile :: Coordinate -> Mountain -> Maybe Tile
lookupTile _ (Mountain []) = Nothing
lookupTile (Coordinate (x, y)) mountain = lookupMountain y mountain >>= lookupHill x

move :: Coordinate -> Coordinate -> Coordinate
move
  (Coordinate (x1, y1))
  (Coordinate (x2, y2)) = Coordinate (x1 + x2, y1 + y2)

-- your initial tile
startTile :: World -> Maybe Tile
startTile World {navigator, plane} = lookupTile (coerce navigator) plane

-- go from one place to another
journey :: Maybe (World, Tile) -> Maybe (World, Tile)
journey Nothing = Nothing
journey (Just (World {plane = Mountain []}, _)) = Nothing
journey (Just (World {navigator, pitch, plane}, _)) =
  let destination = move (coerce navigator) (coerce pitch)
   in do
        fate <- lookupTile destination plane
        Just (World {navigator = (Toboggan destination), pitch, plane}, fate)

-- start your journey
travel :: World -> Maybe (World, Tile)
travel world = do
  origin <- startTile world
  journey $ Just (world, origin)

parseChar :: Char -> Tile
parseChar '#' = Tree
parseChar '.' = Square
parseChar _ = error "could not parse tile"

parseMountain :: [[Char]] -> Mountain
parseMountain mountain =
  coerce $
    cycle . fmap parseChar <$> mountain

readInput :: IO [[Char]]
readInput = lines <$> hGetContents stdin

origin :: Coordinate
origin = Coordinate (0, 0)

interpret :: Mountain -> Slope -> World
interpret plane pitch = World {navigator = Toboggan origin, pitch, plane}

load :: IO Mountain
load = parseMountain <$> readInput

-- to live is to have been to places
life :: World -> [Tile]
life world = do
  voyage <-
    catMaybes $
      takeWhile inBounds $
        iterate journey (travel world)
  return $ snd voyage
  where
    inBounds = isJust

getTrees :: World -> Int
getTrees world = length $ filter (== Tree) $ life world

main :: IO ()
main =
  let run :: Mountain -> Slope -> Int
      run mountain slope = getTrees $ interpret mountain slope

      results :: Mountain -> [Int]
      results mountain =
        run mountain
          <$> [ Slope (Coordinate (1, 1)),
                Slope (Coordinate (3, 1)),
                Slope (Coordinate (5, 1)),
                Slope (Coordinate (7, 1)),
                Slope (Coordinate (1, 2))
              ]
   in do
        mountain <- load

        print $ results mountain
        print . product $ results mountain
