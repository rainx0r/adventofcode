import Data.List
import GHC.Arr

type WordSearchArray = Array (Int, Int) Char

type Coord = (Int, Int)

parseInput :: IO [[Char]]
parseInput = lines <$> readFile "data/day04.txt"

toArray :: [[Char]] -> WordSearchArray
toArray vss = array ((0, 0), (w - 1, h - 1)) [((x, y), v) | (x, vs) <- zip [0 ..] vss, (y, v) <- zip [0 ..] vs]
  where
    w = case vss of
      [] -> 0
      vs : _ -> length vs
    h = length vss

inBounds :: WordSearchArray -> Coord -> Bool
inBounds arr (i, j) = (i >= 0) && (i <= w) && (j >= 0) && (j <= h)
  where
    (_, (w, h)) = bounds arr

coords :: Char -> WordSearchArray -> [Coord]
coords x arr = filter (\c -> (arr ! c) == x) [(i, j) | i <- [0 .. w], j <- [0 .. h]]
  where
    (_, (w, h)) = bounds arr

part1 :: WordSearchArray -> Int
part1 arr = length . concat . filter (not . null) $ map (getXMAS arr) (coords 'X' arr)
  where
    getXMAS arr x = filter (== "XMAS") $ map (map ((!) arr)) $ map (filter (inBounds arr) . take 4 . flip iterate x) directions
    directions =
      [ \(x, y) -> (x + i, y + j)
      | i <- [-1 .. 1],
        j <- [-1 .. 1]
      ]

part2 :: WordSearchArray -> Int
part2 arr = length . filter (\a -> length a > 1) $ map (getX_MAS arr) (coords 'A' arr)
  where
    getX_MAS arr x = filter (\s -> s == "SAM" || s == "MAS") $ map (map ((!) arr) . filter (inBounds arr)) $ map (\i -> i x) directions
    directions =
      [ \(x, y) -> [((x + i), (y + i)) | i <- [-1 .. 1]],
        \(x, y) -> [((x - i), (y + i)) | i <- [-1 .. 1]]
      ]

main :: IO ()
main = do
  input <- parseInput
  let arr = toArray input
  putStrLn $ "Part 1: " ++ (show $ part1 arr)
  putStrLn $ "Part 2: " ++ (show $ part2 arr)
