sumaDzielnikow :: Int -> Int
sumaDzielnikow x =
  sum
    [ d
      | d <- [1 .. x - 1],
        x `mod` d == 0
    ]

paryZaprzyjaznione :: Int -> (Int, Int)
paryZaprzyjaznione n =
  head
    [ (x, y)
      | x <- [n - 1, n - 2 .. 1],
        let y = sumaDzielnikow x,
        y < x,
        sumaDzielnikow y == x
    ]

main :: IO ()
main = do
  putStrLn "Wprowadź n:"
  n <- readLn :: IO Int
  if n < 284
    then putStrLn ("Dla n=" ++ show n ++ " nie ma par liczb zaprzyjaźnionych")
    else do
      print (paryZaprzyjaznione n)
