unique :: [Integer] -> [Integer]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

sort :: [Integer] -> [Integer]
sort [] = []
sort (x : xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)

isRightTriangle :: (Integer, Integer, Integer) -> Bool
isRightTriangle (a, b, c) = a ^ 2 + b ^ 2 == c ^ 2

generateTriples :: Integer -> [(Integer, Integer, Integer)]
generateTriples n = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a + b + c <= n, isRightTriangle (a, b, c)]

generateTriplesSums :: Integer -> [Integer]
generateTriplesSums n = map (\(a, b, c) -> a + b + c) $ generateTriples n

generateTriplesWithMaxSums :: Integer -> [Integer]
generateTriplesWithMaxSums n = sort $ unique $ filter (\x -> length (filter (== x) sums) == maxCount) sums
  where
    sums = generateTriplesSums n
    maxCount = maximum $ map (\x -> length $ filter (== x) sums) sums

main :: IO ()
main = do
  putStrLn "Enter n:"
  n <- readLn :: IO Integer
  putStrLn $ "n=" ++ show n ++ ", result: " ++ show (generateTriplesWithMaxSums n)
