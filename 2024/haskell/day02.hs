parseInput :: IO [[Integer]]
parseInput = map (map read . words) . lines <$> readFile "data/day02.txt"

pairs :: [a] -> [(a, a)]
pairs = zip <*> drop 1

checkReport :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
checkReport xs cond = all (uncurry cond) (pairs xs)

diffIsValid :: Integer -> Integer -> Bool
diffIsValid a b =
  let diff = abs (a - b)
   in diff >= 1 && diff <= 3

isReportIncreasing xs = checkReport xs (>)

isReportDecreasing xs = checkReport xs (<)

hasValidDiffs xs = checkReport xs diffIsValid

isReportSafe :: [Integer] -> Bool
isReportSafe xs = (isReportIncreasing xs || isReportDecreasing xs) && hasValidDiffs xs

isReportSafeDampened :: [Integer] -> Bool
isReportSafeDampened xs =
  let ds = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]
   in any isReportSafe $ [xs] ++ ds

part1 :: IO Integer
part1 = toInteger . length . filter isReportSafe <$> parseInput

part2 :: IO Integer
part2 = toInteger . length . filter isReportSafeDampened <$> parseInput

main :: IO ()
main = do
  part1out <- part1
  putStrLn $ "Part 1: " ++ (show part1out)
  part2out <- part2
  putStrLn $ "Part 2: " ++ (show part2out)
