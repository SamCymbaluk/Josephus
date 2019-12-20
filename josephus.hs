import Test.QuickCheck

{-|
    The 'josephusAux' function takes an integer 'n' and
	an infinite list of the form '[1..n] + [1..n] + ...'
    and computes the Josephus number 'J(n)'.
-}
josephusAux :: Integer -> [Integer] -> Integer
josephusAux 1 (x:xs) = x
josephusAux n (x:(y:xs)) = (josephusAux (n-1) (filter (/= y) xs))
-- ^ The head is popped off and "kills" the second element by removing it from the infinite list
-- ^ This repeats for 'n-1' steps until the list consists of a single Integer repeating infinitely

{-|
    The 'josephus' function takes an integer 'n' and
    computes the Josephus number 'J(n)'.
-}
josephus :: Integer -> Integer
josephus n = josephusAux n (cycle [1..n])

{-|
    The 'josephusNumbers' function returns the infinite list
	of tuples '(n, J(n))' where n is a positive Integer.
-}
josephusNumbers = zip [1..] (map josephus [1..])

{-| 
    The 'josephusFormula' function takes an Integer 'n' and
    uses the general formula to calculate 'J(n)'.
-}
josephusFormula n = 2 * (n - p) + 1
    where p = 2 ^ ((floor . logBase 2 . fromIntegral) n)

-- | QuickCheck test case to ensure the formula matches the algorithmic solution
josephusFormulaProp1 :: Integer -> Bool
josephusFormulaProp1 n 
    | n > 0 = josephus n == josephusFormula n
    | otherwise = True
