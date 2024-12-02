import Data.List

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

part1 :: IO Integer
part1 = sum . map abs . map (foldr (-) 0) . transpose . (map sort) . transpose . map (map read) . (map words) . lines <$> readFile "data/day01.txt"

part2 :: IO Integer
part2 = solve . toTuple . transpose . map (map read) . (map words) . lines <$> readFile "data/day01.txt"
  where
    solve (x1, x2) = sum (zipWith (*) x1 (map (getCountSingle x2) x1))
    getCountSingle yy x = (toInteger . length) (filter (== x) yy)

main :: IO ()
main = do
  part1out <- part1
  putStrLn $ "Part 1: " ++ (show part1out)
  part2out <- part2
  putStrLn $ "Part 2: " ++ (show part2out)
