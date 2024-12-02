parseInput :: IO [[Integer]]
parseInput = map (map read . words) . lines <$> readFile "data/day02.txt"

pairs :: [a] -> [(a, a)]
pairs = zip <*> drop 1

checkReport :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
checkReport xs cond = foldl checkPairs True (pairs xs)
  where
    checkPairs s (x, y) = s && (cond x y)

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
isReportSafeDampened xs = isReportSafe xs || any id ((map isReportSafe) [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]])

part1 :: IO Integer
part1 = sum . map (toInteger . fromEnum . isReportSafe) <$> parseInput

part2 :: IO Integer
part2 = sum . map (toInteger . fromEnum . isReportSafeDampened) <$> parseInput


main :: IO ()
main = do
  part1out <- part1
  putStrLn $ "Part 1: " ++ (show part1out)
  part2out <- part2
  putStrLn $ "Part 2: " ++ (show part2out)
