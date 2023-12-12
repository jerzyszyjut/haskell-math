import Data.List (nub, sort)

-- For a given natural number n, give such a number x ≤ n that can be decomposed into the largest number of
-- different triples a, b, c. Such that a + b + c = x and from sides of length a, b, c can be built a triangle
-- Rectangular. In the case of several maximum values of x, return all of them. For example,
-- for n=100 the numbers 60, 84 and 90 each have 2 distributions. Then the result should be: [60, 84, 90]

isRightTriangle :: (Integer, Integer, Integer) -> Bool
isRightTriangle (a, b, c) = a ^ 2 + b ^ 2 == c ^ 2

generateTriples :: Integer -> [(Integer, Integer, Integer)]
generateTriples n = [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a + b + c <= n, isRightTriangle (a, b, c)]

generateTriplesSums :: Integer -> [Integer]
generateTriplesSums n = sort $ map (\(a, b, c) -> a + b + c) $ generateTriples n

generateTriplesWithMaxSums :: Integer -> [Integer]
generateTriplesWithMaxSums n = nub $ filter (\x -> length (filter (== x) sums) == maxCount) sums
  where
    sums = generateTriplesSums n
    maxCount = maximum $ map (\x -> length $ filter (== x) sums) sums

main :: IO ()
main = do
  -- Input n in the console
  putStrLn "Enter n:"
  n <- readLn :: IO Integer
  putStrLn $ "n=" ++ show n ++ ", result: " ++ show (generateTriplesWithMaxSums n)
