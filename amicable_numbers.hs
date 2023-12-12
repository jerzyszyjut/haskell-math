import Control.Applicative (Alternative(empty))
sumaDzielnikow :: Int -> Int
sumaDzielnikow x = 
 sum (
  [ d | d <- [1..sqrl], x `mod` d == 0, d*d<=x] 
  ++ 
  [ x `div` d | d <- [2 .. sqrl], x `mod` d == 0, d*d<x]
  ) 
 where sqrl = floor (sqrt (fromIntegral x)) + 1

paryZaprzyjaznione :: Int -> [(Int, Int)]
paryZaprzyjaznione n = [ 
 (x, y)
 | x <- [n, n - 1 .. 1],
 let y = sumaDzielnikow x,
 y < x, sumaDzielnikow y == x
 ]

main :: IO ()
main = do
 putStrLn "Wprowadź n:"
 n <- readLn :: IO Int
 let pary = paryZaprzyjaznione n
 if null pary
  then putStrLn ("Dla n=" ++ show n ++ " nie ma par liczb zaprzyjaźnionych")
  else do
   print (head pary)