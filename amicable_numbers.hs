sumaDzielnikow :: Int -> Int
sumaDzielnikow x =
  sum
    [ d
      | d <- [1 .. x - 1],
        x `mod` d == 0
    ]

paryZaprzyjaznione :: Int -> (Int, Int)
paryZaprzyjaznione n
  | n < 284 = (0, 0)
  | otherwise =
      head
        [ (x, y)
          | x <- [n - 1, n - 2 .. 1],
            let y = sumaDzielnikow x,
            y < x,
            sumaDzielnikow y == x
        ]

main :: IO ()
main = do
  putStrLn "Enter n:"
  n <- readLn :: IO Int
  print (paryZaprzyjaznione n)
