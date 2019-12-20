import Test.QuickCheck

josephus :: Integer -> Integer
josephus n = josephusAux n (cycle [1..n])

josephusAux :: Integer -> [Integer] -> Integer
josephusAux 1 (x:xs) = x
josephusAux n (x:(y:xs)) = (josephusAux (n-1) (filter (/= y) xs))

josephus :: Integer -> Integer
josephus n = josephusAux n (cycle [1..n])

josephus_numbers = zip [1..] (map josephus [1..])

josephusFormula n = 2 * (n - p) + 1
    where p = 2 ^ ((floor . logBase 2 . fromIntegral) n)

josephusFormulaProp1 :: Integer -> Bool
josephusFormulaProp1 n 
    | n > 0 = josephus n == josephusFormula n
    | otherwise = True
