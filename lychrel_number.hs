isPalindrome :: String -> Bool
isPalindrome str = str == reverse str                       -- Checks if a string is a palindrome

sumWithReversedNumber :: Integer -> Integer                 -- Sums a number with its reversed number            
sumWithReversedNumber x = x + read (reverse (show x))       -- it converts the number to a string, 
                                                            -- reverses it, and converts it back 
                                                            -- to an integer

isLychrelNumber :: Integer -> Integer -> IO ()              -- initializes the helper function 
isLychrelNumber x n = isLychrelCheck (show x) n 0          -- it coverts x to string and 
                                                            -- passes x as a string, n, 0(iterations count)
                                                            -- to the helper function

isLychrelCheck :: String -> Integer -> Integer -> IO ()     
isLychrelCheck xStr n i
    | i == n && not (isPalindrome xStr) = putStrLn $ "Did not find a palindrome within " ++ show n ++ " iterations."
    | isPalindrome xStr = putStrLn $ "Found a palindrome: " ++ xStr
    | otherwise = isLychrelCheck (show (sumWithReversedNumber (read xStr))) n (i + 1)      --read converts the string to an integer
    where                                                                                   --show converts the integer to a string
        x = read xStr :: Integer

main :: IO ()
main = do
    putStrLn "Enter a number: "
    x <- readLn :: IO Integer
    putStrLn "Enter the maximum number of iterations: "
    n <- readLn :: IO Integer
    isLychrelNumber x n
