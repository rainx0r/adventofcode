import Text.Regex.PCRE

part1 :: IO Integer
part1 = sum . map (product . (map read) . drop 1) . flip (=~) "mul\\((\\d+),(\\d+)\\)" <$> readFile "data/day03.txt"

part2 :: IO Integer
part2 = ((uncurry (+)) . (foldr handleOp (0, 0))) . parse <$> readFile "data/day03.txt"
  where
    parse :: String -> [[String]]
    parse x = x =~ "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)"
    handleOp :: [String] -> (Integer, Integer) -> (Integer, Integer)
    handleOp ["do()", _, _] (totalSum, carry) = ((totalSum + carry), 0)
    handleOp ["don't()", _, _] (totalSum, carry) = (totalSum, 0)
    handleOp [_, num1, num2] (totalSum, carry) = (totalSum, (carry + (read num1) * (read num2)))

main :: IO ()
main = do
  part1out <- part1
  putStrLn $ "Part 1: " ++ (show part1out)
  part2out <- part2
  putStrLn $ "Part 2: " ++ (show part2out)
